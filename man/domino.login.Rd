\name{domino.login}
\alias{domino.login}
\encoding{UTF-8}
\title{
domino.login
}
\description{
Logins to Domino server.
}
\usage{
domino.login(usernameOrEmail, password, approvalForSendingErrorReports=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{usernameOrEmail}{
Login or e-mail address used when registering for Domino Data Lab account. Ex. \code{"jglodek"}
}
  \item{password}{
Secret password that was set for authenticating in Domino Data Lab server. Ex. \code{"my-secret-password"}
}
  \item{approvalForSendingErrorReports}{
Approval for the Domino client to send error reports to Domino in order to improve the product
(these will NEVER include any of your data or source code).
This defaults to FALSE. Ex. \code{ FALSE }
}
}
\examples{
\dontrun{
    domino.login("jglodek","my-super-secret-password", TRUE)}
}
\keyword{ login }

