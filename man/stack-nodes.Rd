% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-links.R
\name{stack_nodes}
\alias{stack_nodes}
\alias{unstack_nodes}
\title{Dynamically group nodes}
\usage{
stack_nodes(stack_id = NULL, nodes = NULL, rv, parent, session)

unstack_nodes(parent, session)
}
\arguments{
\item{stack_id}{Stack id to attach nodes to.}

\item{nodes}{Vector of node ids to stack.}

\item{rv}{Board reactive values.}

\item{parent}{Global scope (entire app) reactive values.}

\item{session}{Shiny session object.}
}
\description{
Given a set of selected nodes, add them to a unique group
and apply unique color and labels.

Useful when removing a stack.
}
\keyword{internal}
