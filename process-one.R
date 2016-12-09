options(repos="http://cran.us.r-project.org")

library(devtools)
library(testthat)
library(optparse)

mkdir <- function(dir, quiet=FALSE, ...) {
    if (!dir.exists(dir)) {
        if (!quiet) {
            cat("Creating", dir, "\n")
        }

        if (!dir.create(dir, ...)) {
            stop("Unable to create ", dir)
        }
    }
    dir
}

check_not_installed_locally <- function(package) {
    pkgs <- installed.packages()
    if (package %in% pkgs[, 1]) {
        stop("Package ", package, " is installed locally in ", pkgs[package, 2])
    }
}

info <- function(fmt, ...) {
    cat(sprintf(paste0(fmt, "\n"), ...))
}

load_genthat <- function(genthat_path, recompile=FALSE) {
    stopifnot(dir.exists(genthat_path))

    info("Loading genthat from %s", genthat_path)

    tryCatch({
        devtools::load_all(genthat_path, recompile=recompile)
    }, error=function(e) {
        print(e)
        message("Unable to load genthat, trying to recompile it")

        devtools::install_dev_deps(genthat_path)
        devtools::load_all(genthat_path, recompile=TRUE)
    })
}

run_phase <- function(name, code) {
    now <- function() as.numeric(Sys.time())

    info("\n----> %s\n", name)
    time <- now()

    # FIXME: there is bug so more nesting will segfault
    # tryCatch({
        r <- code
        time <- now() - time

        info("\n<---- %s finished in %f sec", name, time)
        return(r)
    # }, error=function(e) {
    #     info("\n\n<---- Failure in %s: %s\n\n", name, e$message)
    #     print(e)
    #
    #     stop(e)
    # })
}

do_run_tests <- function(test, file, verbose=TRUE) {
    test_res <- force(test)

    if (verbose) {
        df <- as.data.frame(test_res)
        print(subset(df, select=-c(context, output, user, system, messages)))
        info("tests result (failed/error/all): %d/%d/%d ", sum(df$failed), sum(df$error), sum(df$nb))
    }

    saveRDS(test_res, file=file)
}

run_tests <- function(pkgdir, outdir, verbose=TRUE) {
    do_run_tests(run_phase("test package", testr::test_package(pkgdir)), file=file.path(outdir, "test-results.RDS"), verbose=verbose)
}

run_genthat_tests <- function(pkgdir, genthatdir, outdir, verbose=TRUE) {
    do_run_tests(run_phase("run genthat tests", testr::run_genthat_tests(pkgdir, genthatdir)), file=file.path(outdir, "genthat-test-results.RDS"), verbose=verbose)
}

run_genthat <- function(pkgdir, genthatdir, verbose) {
    mkdir(genthatdir, quiet=!verbose)

    # this is temporar fix to install it into a different directory
    withr::with_temp_libpaths({
        devtools::install_dev_deps(pkgdir)
        install.packages(pkgdir, repos = NULL, quiet=!verbose)

        testr_options("capture.folder", genthatdir)
        testr_options("capture.file", "capture-genthat")

        # FIXME: there is bug so more nesting will segfault
        # run_phase("genthat",
            gen_from_package(
                pkgdir,
                output=genthatdir,
                build=FALSE,
                include.manPages=TRUE,
                include.vignettes=TRUE,
                include.tests=TRUE,
                filter=FALSE,
                verbose=verbose,
                clear_capture=FALSE
            )
        # )
    })
}

save_environment <- function(outdir, ...) {
    envir <- list(
        ...,
        timestamp=format(Sys.time(), "%Y-%m-%d %X"),
        R_version=with(R.version, sprintf("%s.%s %s (%s)", major, minor, version.string, system)),
        R_ENABLE_JIT=as.numeric(Sys.getenv("R_ENABLE_JIT")),
        R_JIT_STRATEGY=as.numeric(Sys.getenv("R_JIT_STRATEGY")),
        installed_packages={
            xs <- installed.packages()
            xs <- xs[, c("Package","Version")]
            xs <- apply(xs, 1, function(x) paste0(x[1], "-", x[2]))
            names(xs) <- NULL
            xs
        }
    )

    print(envir)
    saveRDS(envir, file=file.path(outdir, "envir.RDS"))
}

main <- function() {
    option_list <- list(
        make_option("--clean", action="store_true", default=FALSE, help="Clean"),
        make_option("--all", action="store_true", default=FALSE, help="Run all"),
        make_option("--run-genthat", action="store_true", default=FALSE, help="Run genthat"),
        make_option("--run-genthat-tests", action="store_true", default=FALSE, help="Run genthat tests"),
        make_option("--run-tests", action="store_true", default=FALSE, help="Run package tests"),
        make_option("--genthat", type="character", help="Path to genthat [default %default]", default=file.path(getwd(), "genthat"), metavar="PATH"),
        make_option("--recompile-genthat", action="store_true", default=FALSE, help="Recompile genthat"),
        make_option("--package", type="character", help="Name of the package", metavar="PACKAGE"),
        make_option("--version", type="character", help="Package version", metavar="VERSION"),
        make_option("--outdir", type="character", help="Name of the output directory", metavar="PATH"),
        make_option(c("-v", "--verbose"), action="store_true", default=TRUE, help="Print extra output [default]"),
        make_option(c("-q", "--quietly"), action="store_false", dest="verbose", help="Print little output")
    )

    parser <- OptionParser(option_list=option_list)
    opt <- {
        tryCatch({
            opt <- parse_args(parser)

            stopifnot(!is.null(opt$package))
            check_not_installed_locally(opt$package)

            if (is.null(opt$outdir)) {
                opt$outdir <- paste0("output-", opt$package)
            }

            mkdir(opt$outdir, recursive=TRUE, quiet=!opt$verbose)

            opt
        }, error=function(e) {
            message("Error: ", e$message)
            print_help(parser)
            quit(save="no", status=1)
        })
    }

    run_phase("saving environment", save_environment(opt$outdir, opt=opt))

    all <- opt$all
    genthatdir <- file.path(opt$outdir, "genthat")
    pkgdir <- file.path(opt$outdir, opt$package)

    run_phase("loading genthat", load_genthat(opt$genthat, opt$`recompile-genthat`))

    if (opt$clean) {
        run_phase("cleaning outdir", lapply(c(pkgdir, genthatdir), function(x) {
            if (dir.exists(x)) {
                info("removing %s", x)
                unlink(x, recursive=TRUE)
            }
        }))
    }

    package <- {
        tryCatch(as.package(pkgdir), error=function(e) {
            run_phase("downloading package", testr::download_package(opt$package, opt$outdir, version=opt$version, verbose=opt$verbose, overwrite=TRUE))
        })
    }

    if (all || opt$`run-test`) {
        run_tests(pkgdir, opt$outdir, verbose=opt$verbose)
    }

    if (all || opt$`run-genthat`) {
        run_genthat(pkgdir, genthatdir, verbose=opt$verbose)
    }

    if (all || opt$`run-genthat-test`) {
        if (!dir.exists(genthatdir)) {
            run_genthat(pkgdir, genthatdir, verbose=opt$verbose)
        }

        run_genthat_tests(pkgdir, genthatdir, opt$outdir, verbose=opt$verbose)
    }
}

if (!interactive()) {
    main()
}
