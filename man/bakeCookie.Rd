\name{bakeCookie}
\alias{bakeCookie}
\title{Bake authentication cookies for UDIS}
\description{
  The user authentication in the UDIS system is done by creating
  program-specific \emph{cookies}. User creates a cookie by specifying
  application name, user name and password. Once a cookie is received,
  it can be used for queries until it expires.
}
\usage{
bakeCookie(appname = "udis_expression", user, apppassword, verbose = TRUE)
}
\arguments{
  \item{appname}{Character, application name}
  \item{user}{Character, user name}
  \item{apppassword}{Character, password}
  \item{verbose}{Logical, whether the function should print login
    information (success or failure)?}
}
\details{
  A cookie is a string used to identify user to the remote server. Once
  acquired (\emph{baked}), it can be used many times before expiration.

  If the baking failed, a warning message is printed with potential
  reasons. If this information is not helpful, please contact the UDIS
  developer or the package maintainer.
}
\value{
  A string containing the cookie.
}
\author{
  Jitao David Zhang <jitao_david.zhang@roche.com>, with inputs from Yuan
  Wang, Said Aktas and Clemens Broger.
}
\examples{
\dontrun{
bakeCookie(appname="udis_expression", user="readonly",
apppassword="readonly")
}
}
