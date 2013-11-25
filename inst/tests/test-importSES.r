context("SES Importation")

test_that("ImportSES imports TDR data correctly", {
	ses <- importSES(paste0(.libPaths()[1], "/SES/data/2011-00.mat"), type="tdr")
	
	expect_that(class(ses), equal(c("SES", "list")))
	expect_that(class(ses$tdr), equal(c("tdr", "data.frame")))
	expect_that(!is.null(ses$Ind.id), equals(TRUE))
	
})

test_that("ImportSES imports Statdives data correctly", {
	ses <- importSES(paste0(.libPaths()[1], "/SES/data/2011-00.mat"), type="stat")
	
	expect_that(class(ses), equal(c("SES", "list")))
	expect_that(class(ses$stat), equal(c("statdives", "data.frame")))
	expect_that(!is.null(ses$Ind.id), equals(TRUE))
	
})

test_that("ImportSES imports SES data correctly", {
	ses <- importSES(paste0(.libPaths()[1], "/SES/data/2011-00.mat"))
	
	expect_that(class(ses), equal(c("SES", "list")))
	expect_that(class(ses$tdr), equal(c("tdr", "data.frame")))
	expect_that(class(ses$stat), equal(c("statdives", "data.frame")))
	expect_that(!is.null(ses$Ind.id), equals(TRUE))
	
})