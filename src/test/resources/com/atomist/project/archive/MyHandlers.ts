import {HandleResponse, HandleEvent, Respondable, HandleCommand, Respond, Instruction, Response, CommandContext, Plan, Message} from '@atomist/rug/operations/Handlers'
import {TreeNode, Match, PathExpression} from '@atomist/rug//tree/PathExpression'
import {EventHandler, ResponseHandler, CommandHandler, Parameter, Tags, Intent} from '@atomist/rug/operations/Decorators'
import {Project} from '@atomist/rug/model/Core'

/**

Proposal:
- Rename Handler to EventHandler
- Remove Executors, and introduce CommandHanders and ResponseHandlers

- CommandHandlers, ResponseHandlers and EventHandlers are like rugs in that they
  are addressible in Rug Runner (lives in a Rug archive and has a name etc.)

- CommandHandlers:
   - Invoked by the user via slack/bot (currently)
   - Can register bot intent
   - Can have access to secrets (or perhaps Executions should deal with these?)
   - They are basically the things that @cd uses to do all his fancy botlets.

- EventHandlers
   - These are what the current Handlers are. They determine a plan of action in response to system events.

- ResponseHandlers
   - Are used to handle the responses from other Instructions (Editors, Executions, etc.)

- EventHandlers, ResponseHandlers and CommandHandlers all return a Plan or a Message

- Plans
  - Are pure data responses from CommandHandlers, ResponseHandlers and EventHandlers
  - They can contain things that are 'Plannable'

- Plannable things include: Edit | Generate | Review | Execute | Command
   - These are Instructions used in Plans to address other Rugs (editors, generators etc.)
- Executions represent deferred execution
  of some function that (currently) lives in the JVM in the ExecutionRegistry
  (formally CommandRegistry?).
- Success/Errors from Executions can be handled by registering ResponseHandlers
  These are like callbacks, but are named like other rugs and return Plans too!
- Queries are (nearly) always performed via PathExpressions

- ResponseHandlers
  - Success/Error responses are in a standard format (as per HTTP)
  - code
  - status
  - body - this can be anything at all and is what is returned by the execution of a Rug

**/

//NOTE: use case 1: Reopen an issue twice and handle success/failure

@EventHandler("ClosedIssueReopener","Reopens closed issues",  "/issue")
@Tags("github", "issues")
class SimpleHandler implements HandleEvent<Issue,Issue>{
  handle(match: Match<Issue,Issue>): Plan {
    let issue = match.root()
    let reopen = issue.reopen
    reopen.onSuccess = {name: ""}
  //  reopen.onSuccess = {text: `Issue ${issue.number} has been reopened successfully`}
  //  reopen.onError = {name: "sendFailureMessage", parameters: {issue: issue, who: "jess"}}
    return new Plan().add(reopen)
  }
}

export let simple = new SimpleHandler();

@ResponseHandler("IssueClosedResponder", "Logs failed issue reopen attempts")
class IssueReopenFailedResponder implements HandleResponse<Issue>{

  @Parameter({description: "Name of recipient", pattern: "^.*$"})
  who: string

  handle(response: Response<Issue>): Plan {
    let issue = response.body
    let msg = new Message(`Issue ${issue.number} was not reopened, trying again`)
    msg.channelId = this.who
    return new Plan()
      .add(msg)
  }
}

export let responder = new IssueReopenFailedResponder();

//NOTE use case 2: run an editor/reviewer across a bunch of repos (the old Executor)

@CommandHandler("LicenseAdder","Runs the SetLicense editor on a bunch of my repos")
@Tags("github", "license")
@Intent("add license")
class LicenseAdder implements HandleCommand{

  @Parameter({description: "The name of the license", pattern: "^.*$"})
  license: string;

  handle(command: CommandContext) : Plan {
    let result = new Plan()
    var match: Match<TreeNode,Project>; //command.pathExpressionEngine().evaluate<TreeNode,Project>("/Team()/Owns::Org()/Has::Repo()")
    match.matches().forEach(project => {
      result.add({instruction: {name: "blah", kind: "editor", project: project}})
    })
    return result;
  }
}

export let adder = new LicenseAdder();

//NOTE: use case 3: handle a Command from the bot (via intent or otherwise)
//1. First class intent
//2. First class secrets
//3. As per other Rugs - declared params are passed in to the command

@CommandHandler("ListIssuesHandler","Lists open github issues in slack")
@Tags("github", "issues")
@Intent("list issues")
class IssueLister implements HandleCommand{

  @Parameter({description: "Days", pattern: "^.*$", maxLength: 100, required: false })
  days = 1

  handle(ctx: CommandContext) : Message {
    var match: Match<Issue,Issue>; // ctx.pathExpressionEngine().evaluate<Issue,Issue>("/Repo()/Issue[@raisedBy='kipz']")
    let issues = match.matches();
    if (issues.length > 0) {
              let attachments = `{"attachments": [` + issues.map(i => {
                 let text = JSON.stringify(`#${i.number}: ${i.title}`)
                 if (i.state == "closed") {
                     return `{
                   "fallback": ${text},
                   "author_icon": "http://images.atomist.com/rug/issue-closed.png",
                   "color": "#bd2c00",
                   "author_link": "${i.issueUrl}",
                   "author_name": ${text}
                }`
                 }
                 else {
                     return `{
                 "fallback": ${text},
                 "author_icon": "http://images.atomist.com/rug/issue-open.png",
                 "color": "#6cc644",
                 "author_link": "${i.issueUrl}",
                 "author_name": ${text}
              }`
                 }
             }).join(",") + "]}"
             return {text: attachments}
         }else{
            return {text: "You are not crushin' it right now!"}
         }
  }
}

export let lister = new IssueLister();

// NOTE: use case 4: search youtube for kitty videos and post results to slack
// no params - just take Command
@CommandHandler("ShowMeTheKitties","Search Youtube for kitty videos and post results to slack")
@Tags("kitty", "youtube", "slack")
@Intent("show me kitties")
class KittieFetcher implements HandleCommand{
  handle(command: CommandContext) : Plan {
    let result = new Plan()
    result.add({instruction: {kind: "execution",
                name: "HTTP",
                parameters: {method: "GET", url: "http://youtube.com?search=kitty&safe=true", as: "JSON"}},
                onSuccess: {kind: "respond", name: "Kitties"},
                onError: {text: "No kitties for you today!"}})
    return result;
  }
}

@ResponseHandler("Kitties", "Prints out kitty urls")
class KittiesResponder implements HandleResponse<Object>{
  handle(response: Response<Object>) : Message {
    let results = response.body as any;
    return new Message(results.urls.join(","))
  }
}

export let kittRes = new KittiesResponder();

// stuff associated with types/executions that should have typings

interface Issue extends TreeNode {
  reopen: Respondable<"execute">
  title: string
  number: string
  state: string
  issueUrl: string
}
