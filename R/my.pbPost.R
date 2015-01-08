#' Quick R pushbullet message
#' 
#' Function to post to Pushbullet only if certain conditions are met.  This 
#' allows you to have others run your code on their computers without getting 
#' messages about not having RPushbullet set up.
#' For you to receive a message from this, you must set up your Pushbullet 
#' account with your ".rpushbullet.json" file saved to your home directory.
#' @param message Message you would like sent to your pushbullet account.
#' @param type Type of message to send. I always use "note," but others may be
#' possible.
#' @param title Title of message. (Note: if you are receiving pushbullets on 
#' your iPhone, the title will be visible from your lock screen.)
#' @keywords pushbullet, error messages
#' @seealso pbPost
#' @export
#' @examples
#' ## This sends a PB if set up on the computer
#' my.pbPost("body of test message")
#' 
my.pbPost <- function(message, type="note", title="R message"){
    if (require(RPushbullet) & file.exists("~/.rpushbullet.json")){
        suppressWarnings(pbPost(type,title,message))
    }
}
