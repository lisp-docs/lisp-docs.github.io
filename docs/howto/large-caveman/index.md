# Structuring A Large caveman2 Project

## How To Structure a Large caveman2 Project Into Multiple Modules?

How can I go about splitting large parts of a web app into modules to make it more manageable for writing `.asd` files and separating functionality.

This is to avoid importing every file in the `.asd` file and prevent it from becoming bloated and unmanageable.

For example, say I want to have an authentication module which has its own classes, routes, and views: How can I define it in a system say in `/src/auth/auth.asd` and be able to use those routes in the main web app by importing it?

To use another package to define routes is itself not so clearly documented: https://stackoverflow.com/questions/76330818/how-to-define-caveman2-routes-in-multiple-files-packages/76332385#76332385

But given that defroute is expecting an `*app*` it looks like an app should be created as a caveman2 project, and then modules created outside of the project, and then another system should import the caveman2 project as a system, and then the other modules would depend on the caveman2 project as a base.

## Answer

The way to do it is to add a `my-project.base.asd` package which contains

- `config.lisp`
- `db.lisp`
- `view.lisp`
- `web.lisp`
- `main.lisp`

Then the original will contain the dependency on this `my-project.base.asd` and any other asd projects you declare. That way you can make smaller system definition files and encourage proper breaking up of the application code into more independent modules you can reuse over applications.

A key thing to think about here if you want to make your modules really independent is to not make them depend on the base system. For controllers, it gets tricky because of the macro `defroute`. So probably a good approach is make a new macro that will instead make a list of routes to be defined once a `*app*` variable is provided, and then call the ningle function to register a route (which is what defroute does when macroexpanded) on all those routes to be defined.

So the `my-project.asd` file will have a few dependencies:

- `my-project.base`
- `my-project.auth` (for example of an independent module which would declare routes etc)
- `my-project.main` which would then tie in auth into the project and would call some function/macro defined in the `my-project.auth` package and pass it the `my-project.web:*web*` app variable and define all relevant routes.

Note that in this example, if you are defining some `user` class or anything that is persisted on the db, the `my-project.main` will also be responsible for executing the code to persist that class exported or extend it applying specific project related data and then persist it.

## Further Reading

Note that this advice is following the [more traditional common lisp way of structuring projects](/docs/tutorial/projects/structuring_large_projects). Check out the [package inferred systems](/docs/tutorial/projects/package_inferred) guide for a different approach.
