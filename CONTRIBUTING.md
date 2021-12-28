# Contributing to Grell-Freitas-Physics

NOTE: If you are reading this with a plain text editor, please note that this 
document is formatted with Markdown syntax elements.  See 
https://www.markdownguide.org/cheat-sheet/ for more information. It is
recommended to view [this document](https://github.com/NOAA-GSL/SENA-c_Grell-Freitas-Physics/blob/develop/CONTRIBUTING.md)
on GitHub.


## Contents

[How to Contribute](#how-to-contribute)

[Contributing an Alternative Implementation](#contributing-an-alternative-implementation)

[Branch Management](#branch-management)

[Pull Request Rules and Guidelines](#pull-request-rules-and-guidelines)

[Fortran Style Guide](#fortran-style-guide)

## How to Contribute

Contributions to Grell-Freitas-Physics will be accepted via [pull request](https://docs.github.com/en/free-pro-team@latest/github/collaborating-with-issues-and-pull-requests/creating-a-pull-request),
either from branches in this repository, or from a fork of this repository. Pull
requests will be reviewed and evaluated based on the technical merit of the
proposed changes as well as conformance to the style guidelines outlined in this
document. Code reviewers may request changes as a condition of acceptance of the
pull request.

## Contributing an Alternative Implementation

Contributors who wish to provide an alternative implementation of `Grell-Freitas-Physics`
can do so by issuing a pull request to the `develop` branch from their
own forks of this repository. Due to NOAA policy, write access to this
repository can not be granted for external collaborators.

All pull requests for new implementations should include:

* A portable build system that functions across platforms and (if applicable)
across multiple compilers.
* Source code that is well written, properly formatted, and documented.
* A test suite that verifies the implementation produces the same results as
the reference implementation. At present, 11 digits of accuracy are expected.
* A GitHub Actions continuous integeration configuration to automate the new
implementation's test suite (we can help with this if needed).
* A documentation README.md describing the new implementation, including how
to run and test it.

## Branch Management

External collaborators are, of course, free to use the branch management
strategy of their choice in their own forks.  This section applies to branches
created in this repository by internal collaborators, but exertnal collaborators
are encouraged to adopt a similar approach.

This repository follows the [GitHub Flow](https://guides.github.com/introduction/flow/)
branching model with the following modifications borrowed from
[Git Flow](https://nvie.com/posts/a-successful-git-branching-model/):

* All branches that add new features, capabilities, or enhancements must be named:
`feature/name-of-my-feature`
* All branches that fix defects must be named: `bugfix/name-of-my-bugfix`

A rule of thumb for choosing a branch type: If it isn't a bugfix, it is a feature.

In addition to the naming conventions, all branches must have a clearly defined,
singular purpose, described by their name. It is also prefered, but not required,
that branches correspond to an issue documented in the issue tracking system. (Issues
can be added after branch creation, if necessary.)  All branches shall result in a
pull request and shall be deleted immediately after that pull request is merged.

## Pull Request Rules and Guidelines

We ask contributors to please be mindful of the following when submitting a Pull
Request. The following will help reviewers give meaningful and timely feedback.

In order to maintain quality standards, the following rules apply:

* Pull requests will not be accepted for branches that are not up-to-date with
the current `develop` branch.
* Pull requests will not be accepted unless all tests pass.  This include manual
execution of test suites on local platforms as well as the automated continuous
integration tests.
* Pull requests that add new capabilities will not be accepted without the inclusion
of tests that verify those capabilities work properly. (We can help with this if needed)

We also ask contributors to consider the following when proposing changes:

* Provide a thorough explanation of the changes you are proposing and reference any
issues they resolve. Also link to other related or dependent pull requests in
the description.
* Pull requests should be as small as possible. Break large changes into multiple
smaller changes if possible. Put yourself in the reviewers' shoes. If your pull
request is too big, reviewers may ask you to break it into smaller, more digestable,
pieces.
* Group changes that logically contribute to the branch's singular purpose
together.
* Do not group unrelated changes together; create separate branches and submit
pull requests for unrelated changes separately.

## Fortran Style Guide

Unfortunately, there appears not to be a reliable linter for Fortran that can be
used to automate conformance of Fortran coding style. The code in this repository
should use a consistent style throughout. The code should appear as if it were 
written by a single person.

NOTE: There is a tool, [fprettify](https://github.com/pseewald/fprettify), for automatically
formatting Fortran code. However, its output does not comply with this style guide
so we do not recommend its use for contributions to this repository.

We ask collaborators to follow these style guidelines when modifying existing code,
or contributing new code to this repository. Pull request reviewers may ask
collaborators to fix style violations as a condition of approval.

* All Fortran reserved words will be all uppercase

  ```
  ! Use this
  PROGRAM foo
    INTEGER :: foobar
    INTEGER :: foo_bar  ! This is also okay
    INTEGER :: fooBar   ! This is also okay
  END PROGRAM foo

  ! Instead of this
  program foo
    integer :: foobar
  end program foo
  ```

* Use two spaces to indent all code inside `PROGRAM`

  ```
  ! Use this
  PROGRAM foo
    INTEGER :: foobar
  END PROGRAM foo

  ! Instead of this
  PROGRAM foo
  INTEGER :: foobar
  END PROGRAM foo

  ! Please do not use this, either
  PROGRAM foo
        INTEGER :: foobar
  END PROGRAM foo
  ```

* Use two spaces to indent all code inside `MODULE`

  ```
  ! Use this
  MODULE foo
    INTEGER :: foo
  CONTAINS
    SUBROUTINE bar
    END SUBROUTINE bar
  END MODULE foo
  
  ! Instead of this
  MODULE foo
  INTEGER :: foo
  CONTAINS
  SUBROUTINE bar
  END SUBROUTINE bar
  END MODULE foo

* Use two spaces to indent all code inside `SUBROUTINE` and `FUNCTION`

  ```
  ! Use this
  SUBROUTINE foo(bar)
    INTEGER :: bar
    bar = bar + 1
  END SUBROUTINE bar

  ! Instead of this
  SUBROUTINE foo(bar)
  INTEGER :: bar
  bar = bar + 1
       bar = bar + 1  ! Please do not do this, either
  END SUBROUTINE bar
  ```

* Use two spaces to indent all code inside `IF`, `DO`, `WHILE`, etc.

  ```
  ! Use this
  IF (bar > 1) THEN
    DO WHILE (bar < 10)
      WRITE(*, *) "Bar"
      bar = bar + 1
    END DO
  END IF

  ! Instead of this
  IF (bar > 1) THEN
  DO WHILE (bar < 10)
  WRITE(*, *) "Bar"
  bar = bar + 1
  END DO
  END IF

  ! Please do not do this, either
  IF (bar > 1) THEN
       DO WHILE (bar < 10)
            WRITE(*, *) "Bar"
            bar = bar + 1
       END DO
  END IF
  ```

* Use spaces after commas

  ```
  ! Use this
  WRITE(*, '(A, I)') "The number is", a(i, j)

  ! Instead of this
  WRITE(*,'(A,I)') "The number is",a(i,j)
  ```
  
* Use spaces around operators

  ```
  ! Use this
  x = a(i, j) * 1.0 - pi / (rho + phi)

  ! Instead of this
  x=a(i,j)*1.0-pi/(rho+phi)
  ```

* Do NOT use spaces before the open parenthesis when calling a FUNCTION

  ```
  ! Use this
  WRITE(*, *) "Foo"
  CALL bar(x)

  ! Instead of this
  WRITE (*, *) "Foo"
  CALL bar (x)
  ```

* Align variable and intent declarations

  ```
  ! Use this
  SUBROUTINE foo(x, y, z)
    INTEGER, INTENT(   IN) :: x
    REAL,    INTENT(  OUT) :: y
    LOGICAL, INTENT(INOUT) :: z

    REAL, ALLOCATABLE :: foobar(:,:)
    REAL              :: baz
    INTEGER           :: zap

  ! Instead of this
  SUBROUTINE foo(x, y, z)
    INTEGER, INTENT(IN) :: x
    REAL, INTENT(OUT) :: y
    LOGICAL, INTENT(INOUT) :: z

    REAL, ALLOCATABLE :: fobar(:,:)
    REAL :: baz
    INTEGER :: zap
  ```

* Declare SUBROUTINE arguments in the same order they appear in the argument list

  ```
  ! Use this
  SUBROUTINE foo(a, b, c)
    INTEGER :: a
    REAL    :: b
    LOGICAL :: c

  ! Instead of this
  SUBROUTINE foo(a, b, c)
    LOGICAL :: c
    INTEGER :: a
    REAL    :: b    
  ```

* Specify full name in `END` statements

  ```
  ! Use this
  PROGRAM foo
  END PROGRAM foo

  ! Instead of this
  PROGRAM foo
  END PROGRAM
  ```

  And

  ```
  ! Use this
  MODULE foo
    SUBROUTINE bar
    END SUBROUTINE bar
  END MODULE foo

  ! Instead of this
  MODULE foo
    SUBROUTINE bar
    END SUBROUTINE
  END MODULE
  ```

* Use comment header blocks above subroutines

```
! Use this
!****************************************
!
! foo
!
! Description of what foo does.
!
!****************************************
SUBROUTINE foo()

! Instead of this
SUBROUTINE foo()
```
