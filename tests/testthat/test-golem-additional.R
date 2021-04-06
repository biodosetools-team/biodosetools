# {golem} UI HTML utils ----

test_that("list_to_li works", {
  expect_equal(
    as.character(list_to_li(c("a", "b"))),
    "<li>a</li>\n<li>b</li>"
  )
  expect_equal(
    as.character(list_to_li(c("a", "b"), class = "my-class")),
    '<li class="my-class">a</li>\n<li class="my-class">b</li>'
  )
})

test_that("list_to_p works", {
  expect_equal(
    as.character(list_to_p(c("a", "b"))),
    "<p>a</p>\n<p>b</p>"
  )
  expect_equal(
    as.character(list_to_p(c("a", "b"), class = "my-class")),
    '<p class="my-class">a</p>\n<p class="my-class">b</p>'
  )
})

test_that("named_to_li works", {
  expect_equal(
    as.character(named_to_li(c(a = "a", b = "b"))),
    "<li><b>a:</b> a</li>\n<li><b>b:</b> b</li>"
  )
  expect_equal(
    as.character(named_to_li(c(a = "a", b = "b"), class = "my-class")),
    '<li class="my-class"><b>a:</b> a</li>\n<li class="my-class"><b>b:</b> b</li>'
  )
})

test_that("with_red_star works", {
  expect_equal(
    as.character(with_red_star("Enter your name here")),
    '<span>Enter your name here<span style="color:red">*</span></span>'
  )
})

test_that("rep_br works", {
  expect_equal(
    as.character(rep_br(5)),
    "<br/> <br/> <br/> <br/> <br/>"
  )
})

test_that("enurl works", {
  expect_equal(
    as.character(enurl("https://www.thinkr.fr", "ThinkR")),
    '<a href="https://www.thinkr.fr">ThinkR</a>'
  )
})

test_that("jq_hide works", {
  expect_equal(
    as.character(jq_hide("element-id")),
    "<script>$('#element-id').hide()</script>"
  )
})

test_that("tagRemoveAttributes and (un)display work", {
  a <- shiny::tags$p(src = "plop", "pouet")
  a_notag <- tagRemoveAttributes(a, "src")
  a_hidden <- undisplay(a)
  a_unhidden <- display(a_hidden)

  # Expected outcomes
  expect_equal(as.character(a), '<p src="plop">pouet</p>')
  expect_equal(as.character(a_notag), "<p>pouet</p>")
  expect_equal(as.character(a_hidden), '<p src="plop" style="display: none;">pouet</p>')
  expect_equal(as.character(a_unhidden), '<p src="plop" style="">pouet</p>')
})
