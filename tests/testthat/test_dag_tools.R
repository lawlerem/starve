test_that("Lowest k of vector", {
  expect_equal(
    lowest_k(1:10, 5),
    1:5 - 1
  )
  expect_equal(
    lowest_k(
      c(1, 3.5, -1.2, 4, 2),
      1
    ),
    c(3, 1, 5, 2, 4)[1] - 1
  )
  expect_equal(
    lowest_k(
      c(1, 3.5, -1.2, 4, 2),
      2
    ),
    c(3, 1, 5, 2, 4)[1:2] - 1
  )
  expect_equal(
    lowest_k(
      c(1, 3.5, -1.2, 4, 2),
      3
    ),
    c(3, 1, 5, 2, 4)[1:3] - 1
  )
  expect_equal(
    lowest_k(
      c(1, 3.5, -1.2, 4, 2),
      4
    ),
    c(3, 1, 5, 2, 4)[1:4] - 1
  )
  expect_equal(
    lowest_k(
      c(1, 3.5, -1.2, 4, 2),
      5
    ),
    c(3, 1, 5, 2, 4)[1:5] - 1
  )
  expect_equal(
    lowest_k(1:5, 0),
    integer(0)
  )
  expect_error(
    lowest_k(1:10, 11),
    "k is greater"
  )
  expect_error(
    lowest_k(
      c("a", "b", "c"),
      2
    ),
    "Not compatible"
  )
})



test_that("Order distance matrix", {
  expect_equal(
    order_d_matrix(
      rbind(
        c(1, 2, 3),
        c(4, 5, 6),
        c(7, 8, 9)
      )
    ),
    c(1, 2, 3) - 1
  )
  expect_equal(
    order_d_matrix(
      rbind(
        c(0, 1, 3),
        c(1, 0, 4),
        c(3, 4, 0)
      )
    ),
    c(1, 2, 3) - 1
  )
  expect_equal(
    order_d_matrix(
      rbind(
        c(0, 3, 1),
        c(3, 0, 4),
        c(4, 3, 0)
      )
    ),
    c(1, 3, 2) - 1
  )

  expect_equal(
    order_d_matrix(
      as.matrix(
        dist(1:5)
      )
    ),
    1:5 - 1
  )
  expect_equal(
    order_d_matrix(
      as.matrix(
        dist(5:1)
      )
    ),
    1:5 - 1
  )
  expect_equal(
    order_d_matrix(
      as.matrix(
        dist(
          c(1, 3, 4, 5, 2)
        )
      )
    ),
    c(1, 5, 2, 3, 4) - 1
  )
  expect_equal(
    order_d_matrix(
      as.matrix(
        dist(
          c(1, 3, 5, 2, 4)
        )
      )
    ),
    c(1, 4, 2, 5, 3) - 1
  )
  expect_equal(
    order_d_matrix(
      as.matrix(
        dist(
          c(3, 2, 4, 5, 1)
        )
      )
    ),
    c(1, 2, 3, 4, 5) - 1
  )
  expect_equal(
    order_d_matrix(
      as.matrix(
        dist(
          c(3, 2, 5, 1, 1, 4)
        )
      )
    ),
    c(1, 2, 4, 5, 6, 3) - 1
  )
  expect_equal(
    order_d_matrix(
      as.matrix(
        dist(
          c(3, 2, 5, 1, 1, 4, -1 )
        )
      )
    ),
    c(1, 2, 4, 5, 6, 3, 7) - 1
  )

  expect_error(
    order_d_matrix(
      rbind(
        c("a", "b"),
        c("c", "d")
      )
    ),
    "Not compatible"
  )
})


test_that("Distance matrix to dag", {
  d<- unname(as.matrix(dist(1:5)))
  order<- order_d_matrix(d)
  d<- d[order + 1, order + 1]
  e<- list(
    order = order,
    edge_list = list(
      list(to = 0:1, from = integer(0)),
      list(to = 2, from = 1:0),
      list(to = 3, from = 2:1),
      list(to = 4, from = 3:2)
    ),
    dist_list = list(
      d[1:2, 1:2],
      d[1:3, 1:3],
      d[2:4, 2:4],
      d[3:5, 3:5]
    )
  )
  expect_equal(
    dist_to_dag(d, 2),
    e
  )

  d<- unname(
    as.matrix(
      dist(
        c(3, 2, 5, 1, 1, 4, -1)
      )
    )
  )
  # 3 2 1 1 4 5 -1
  order<- order_d_matrix(d)
  d<- d[order + 1, order + 1]
  e<- list(
    order = order_d_matrix(d),
    edge_list = list(
      list(to = 0:2, from = integer(0)),
      list(to = 3, from = c(2, 1, 0)),
      list(to = 4, from = c(0, 1, 2)),
      list(to = 5, from = c(4, 0, 1)),
      list(to = 6, from = c(2, 3, 1))
    ),
    dist_list = list(
      d[c(1, 2, 3), c(1, 2, 3)],
      d[c(4, 3, 2, 1), c(4, 3, 2, 1)],
      d[c(5, 1, 2, 3), c(5, 1, 2, 3)],
      d[c(6, 5, 1, 2), c(6, 5, 1, 2)],
      d[c(7, 3, 4, 2), c(7, 3, 4, 2)]
    )
  )
  expect_equal(
    dist_to_dag(d, 3),
    e
  )
})


test_that("Dist_to_tg_dag", {
  pg_locs<- 1:5
  tg_locs<- c(0, 1.5, 3.5, 6)
  d<- unname(as.matrix(dist(c(tg_locs, pg_locs))))

  tg_idx<- seq_along(tg_locs)
  pg_idx<- length(tg_locs) + seq_along(pg_locs)

  dag<- starve:::dist_to_tg_dag(
    d[tg_idx, pg_idx],
    d[pg_idx, pg_idx],
    3
  )

  parents<- do.call(rbind, lapply(dag$edge_list, `[[`, "from"))
  eparents<- rbind(
    c(0, 1, 2), # 0 : 1, 2, 3
    c(0, 1, 2), # 1.5 : 1, 2, 3
    c(2, 3, 1), # 3.5 : 3, 4, 2
    c(4, 3, 2) # 6 : 5, 4, 3
  )
  expect_equal(
    parents,
    eparents
  )

  ad<- dag$dist_list
  ed<- list(
    rbind( # 0 : 1, 2, 3
      c(0, 1, 2, 3),
      c(1, 0, 1, 2),
      c(2, 1, 0, 1),
      c(3, 2, 1, 0)
    ),
    rbind( # 1.5 : 1, 2, 3
      c(0, 0.5, 0.5, 1.5),
      c(0.5, 0, 1, 2),
      c(0.5, 1, 0, 1),
      c(1.5, 2, 1, 0)
    ),
    rbind( # 3.5 : 3, 4, 2
      c(0, 0.5, 0.5, 1.5),
      c(0.5, 0, 1, 1),
      c(0.5, 1, 0, 2),
      c(1.5, 1, 2, 0)
    ),
    rbind( # 6 : 5, 4, 3
      c(0, 1, 2, 3),
      c(1, 0, 1, 2),
      c(2, 1, 0, 1),
      c(3, 2, 1, 0)
    )
  )
  expect_equal(
    ad,
    ed
  )
})



test_that("Construct_transient_dag", {
  frompoints<- cbind(1:5, 0)
  frompoints<- apply(
    frompoints,
    1,
    sf::st_point,
    simplify = FALSE
  )
  frompoints<- sf::st_sf(geom = sf::st_sfc(frompoints))

  topoints<- cbind(
    c(0, 1.5, 3.5),
    0
  )
  topoints<- apply(
    topoints,
    1,
    sf::st_point,
    simplify = FALSE
  )
  topoints<- sf::st_sf(geom = sf::st_sfc(topoints))
  edges<- list(
    list(to = 1, from = c(1, 2, 3)), # 0
    list(to = 2, from = c(1, 2, 3)), # 1.5
    list(to = 3, from = c(3, 4, 2)) # 3.5
  )
  dists<- list(
    # 0 1 2 3
    rbind(
      c(0, 1, 2, 3),
      c(1, 0, 1, 2),
      c(2, 1, 0, 1),
      c(3, 2, 1, 0)
    ),
    # 1.5 1 2 3
    rbind(
      c(0, 0.5, 0.5, 1.5),
      c(0.5, 0, 1, 2),
      c(0.5, 1, 0, 1),
      c(1.5, 2, 1, 0)
    ),
    # 3.5 3 4 2
    rbind(
      c(0, 0.5, 0.5, 1.5),
      c(0.5, 0, 1, 1),
      c(0.5, 1, 0, 2),
      c(1.5, 1, 2, 0)
    )
  )
  e<- new(
    "dag",
    edges = edges,
    distances = dists,
    distance_units = "m"
  )
  settings<- new("settings")
  settings@n_neighbours<- 3
  settings@distance_units<- "m"

  expect_equal(
    construct_transient_graph(
      topoints,
      frompoints,
      settings = settings
    ),
    e
  )

  edges<- list(
    list(to = 1, from = c(1, 2, 3)),
    list(to = 1, from = c(1, 2, 3)),
    list(to = 2, from = c(3, 4, 2))
  )
  e<- new(
    "dag",
    edges = edges,
    distances = dists,
    distance_units = "m"
  )
  expect_equal(
    construct_transient_graph(
      topoints,
      frompoints,
      time = c(0, 1, 1),
      settings = settings
    ),
    e
  )
})
