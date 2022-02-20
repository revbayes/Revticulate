library(testthat)
library(Revticulate)
test_that(
  "Testing coerceRev()",
  {
    skip_on_os("windows")

    clearRev()

    nums <- unlist(runif(10))

    expect_equal(Revticulate::coerceRev("[" %+% toString(nums) %+% "]"), nums)

    expect_equal(Revticulate::coerceRev("[[" %+% toString(nums) %+% "]," %+% toString(nums) %+% "]]"), list(nums, nums))

    expect_equal(suppressWarnings(Revticulate::coerceRev("myVar")), "myVar")

    expect_equal(ape::read.tree(text = "   ((((Taxon_1[&index=16]:1.000000,Taxon_2[&index=15]:1.000000)[&index=17]:1.000000,(Taxon_3[&index=14]:1.000000,Taxon_4[&index=13]:1.000000)[&index=18]:1.000000)[&index=19]:1.000000,((Taxon_5[&index=12]:1.000000,Taxon_6[&index=11]:1.000000)[&index=20]:1.000000,(Taxon_7[&index=10]:1.000000,Taxon_8[&index=9]:1.000000)[&index=21]:1.000000)[&index=22]:1.000000)[&index=23]:1.000000,(((Taxon_9[&index=8]:1.000000,Taxon_10[&index=7]:1.000000)[&index=24]:1.000000,(Taxon_11[&index=6]:1.000000,Taxon_12[&index=5]:1.000000)[&index=25]:1.000000)[&index=26]:1.000000,((Taxon_13[&index=4]:1.000000,Taxon_14[&index=3]:1.000000)[&index=27]:1.000000,(Taxon_15[&index=2]:1.000000,Taxon_16[&index=1]:1.000000)[&index=28]:1.000000)[&index=29]:1.000000)[&index=30]:1.000000)[&index=31]:0.000000;"),
                 coerceRev("   ((((Taxon_1[&index=16]:1.000000,Taxon_2[&index=15]:1.000000)[&index=17]:1.000000,(Taxon_3[&index=14]:1.000000,Taxon_4[&index=13]:1.000000)[&index=18]:1.000000)[&index=19]:1.000000,((Taxon_5[&index=12]:1.000000,Taxon_6[&index=11]:1.000000)[&index=20]:1.000000,(Taxon_7[&index=10]:1.000000,Taxon_8[&index=9]:1.000000)[&index=21]:1.000000)[&index=22]:1.000000)[&index=23]:1.000000,(((Taxon_9[&index=8]:1.000000,Taxon_10[&index=7]:1.000000)[&index=24]:1.000000,(Taxon_11[&index=6]:1.000000,Taxon_12[&index=5]:1.000000)[&index=25]:1.000000)[&index=26]:1.000000,((Taxon_13[&index=4]:1.000000,Taxon_14[&index=3]:1.000000)[&index=27]:1.000000,(Taxon_15[&index=2]:1.000000,Taxon_16[&index=1]:1.000000)[&index=28]:1.000000)[&index=29]:1.000000)[&index=30]:1.000000)[&index=31]:0.000000;"))

    for(i in 1:10){
      num <- runif(1, 0, 10000000)
      expect_equal(coerceRev(num), num)
    }

    for(i in 1:10){
      nonvar <- paste(LETTERS[as.integer(runif(10)*26)], collapse = "")
      expect_warning(doRev(nonvar, coerce = TRUE), "Missing Variable: Variable " %+% nonvar %+% " does not exist")
    }

    for(i in 1:10){
      pow <- 2**i
      expect_s3_class(doRev("simTree(" %+% pow %+% ")", coerce = TRUE), class = "phylo")
    }

    clearRev()
  }
)



