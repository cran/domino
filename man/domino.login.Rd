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
domino.login(usernameOrEmail, password)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{usernameOrEmail}{
Login or e-mail address used when registering for Domino Data Lab account. Ex. \code{"jglodek"}
}
  \item{password}{
Secret password that was set for authenticating in Domino Data Lab server. Ex. \code{"my-secret-password"}
}
}
\examples{
\dontrun{
    domino.login("jglodek","my-super-secret-password")}
}
\keyword{ login }

