test_that("Add random effects by time",{
  data<- cbind(1:5,0)
  data<- apply(data,1,sf::st_point,simplify=FALSE)
  data<- sf::st_sfc(data)
  suppressWarnings({
    data<- sf::st_sf(
      y = 0,
      t = rep(1:10,each=5),
      geom = data
    )
  })
  init<- prepare_staRVe_model(y~time(t),data)

  re<- stars::st_as_stars(
    list(w = array(0,dim=c(5,10,1)),
         se = array(0,dim=c(5,10,1))),
    dimensions = stars::st_dimensions(
      geom = sf::st_sfc(unique(sf::st_geometry(data))),
      t = 1:10,
      variable = "y"
    )
  )
  te<- stars::st_as_stars(
    list(w = array(0,dim=c(10,1)),
         se = array(0,dim=c(10,1))),
    dimensions = stars::st_dimensions(
      t = 1:10,
      variable = "y"
    )
  )
  expect_equal(random_effects(.add_random_effects_by_time(init,1:10)),re)
  expect_equal(time_effects(.add_random_effects_by_time(init,1:10)),te)


  re<- stars::st_as_stars(
    list(w = array(0,dim=c(5,14,1)),
         se = array(0,dim=c(5,14,1))),
    dimensions = stars::st_dimensions(
      geom = sf::st_sfc(unique(sf::st_geometry(data))),
      t = seq(-3,10),
      variable = "y"
    )
  )
  te<- stars::st_as_stars(
    list(w = array(0,dim=c(14,1)),
         se = array(0,dim=c(14,1))),
    dimensions = stars::st_dimensions(
      t = seq(-3,10),
      variable = "y"
    )
  )
  expect_equal(random_effects(.add_random_effects_by_time(init,seq(-3,0))),re)
  expect_equal(time_effects(.add_random_effects_by_time(init,seq(-3,0))),te)


  re<- stars::st_as_stars(
    list(w = array(0,dim=c(5,14,1)),
         se = array(0,dim=c(5,14,1))),
    dimensions = stars::st_dimensions(
      geom = sf::st_sfc(unique(sf::st_geometry(data))),
      t = 1:14,
      variable = "y"
    )
  )
  te<- stars::st_as_stars(
    list(w = array(0,dim=c(14,1)),
         se = array(0,dim=c(14,1))),
    dimensions = stars::st_dimensions(
      t = 1:14,
      variable = "y"
    )
  )
  expect_equal(random_effects(.add_random_effects_by_time(init,11:14)),re)
  expect_equal(time_effects(.add_random_effects_by_time(init,11:14)),te)


  re<- stars::st_as_stars(
    list(w = array(0,dim=c(5,18,1)),
         se = array(0,dim=c(5,18,1))),
    dimensions = stars::st_dimensions(
      geom = sf::st_sfc(unique(sf::st_geometry(data))),
      t = seq(-3,14),
      variable = "y"
    )
  )
  te<- stars::st_as_stars(
    list(w = array(0,dim=c(18,1)),
         se = array(0,dim=c(18,1))),
    dimensions = stars::st_dimensions(
      t = seq(-3,14),
      variable = "y"
    )
  )
  expect_equal(random_effects(.add_random_effects_by_time(init,c(-3,14))),re)
  expect_equal(time_effects(.add_random_effects_by_time(init,c(-3,14))),te)
})


test_that("Locations from stars",{
  xp<- cbind(1:5,0)
  xp<- apply(xp,1,sf::st_point,simplify=FALSE)
  xp<- sf::st_sfc(xp)

  yp<- cbind(0,1:5)
  yp<- apply(yp,1,sf::st_point,simplify=FALSE)
  yp<- sf::st_sfc(yp)

  st<- stars::st_as_stars(
    list(w = array(0,dim=c(5,1,1))),
    dimensions = stars::st_dimensions(
      xp = xp,
      time = 0,
      variable = "y"
    )
  )
  expect_equal(.locations_from_stars(st),sf::st_sf(xp))

  st<- stars::st_as_stars(
    list(w = array(0,dim=c(5,5,1,1))),
    dimensions = stars::st_dimensions(
      xp = xp,
      yp = yp,
      time = 0,
      variable = "y"
    )
  )
  suppressWarnings(expect_equal(.locations_from_stars(st),sf::st_sf(xp)))

  st<- stars::st_as_stars(
    list(w = array(0,dim=c(5,5,1,1))),
    dimensions = stars::st_dimensions(
      yp = yp,
      xp = xp,
      time = 0,
      variable = "y"
    )
  )
  suppressWarnings(expect_equal(.locations_from_stars(st),sf::st_sf(yp)))

  st<- stars::st_as_stars(
    list(w = array(0,dim=c(1,5,1,5))),
    dimensions = stars::st_dimensions(
      time = 0,
      xp = xp,
      variable = "y",
      yp = yp
    )
  )
  suppressWarnings(expect_equal(.locations_from_stars(st),sf::st_sf(xp)))

  st<- stars::st_as_stars(
    list(w = array(0,dim=c(1,1))),
    dimensions = stars::st_dimensions(
      time = 0,
      variable = "y"
    )
  )
  expect_error(.locations_from_stars(st))
})


test_that("sf from raster list",{
  r1<- raster::raster(nrows=5,ncols=5,xmn=0,xmx=1,ymn=0,ymx=1)
  sf1<- cbind(rep(seq(0.1,0.9,by=0.2),5),rep(seq(0.9,0.1,by=-0.2),each=5))
  sf1<- apply(sf1,1,sf::st_point,simplify=FALSE)
  sf1<- sf::st_sfc(sf1)

  single<- r1; single$T1<- 1;
  good<- r1; good$T1<- 1; good$T2<- 2; good$T3<- 3
  notAllYears<- r1; notAllYears$T1<- 1; notAllYears$T2<- 2
  layerX<- r1; layerX$X1<- 1; layerX$X2<- 2; layerX$X3<- 3

  r2<- raster::raster(nrows=10,ncols=10,xmn=0,xmx=1,ymn=0,ymx=1)
  sf2<- cbind(rep(seq(0.05,0.95,by=0.1),10),rep(seq(0.95,0.05,by=-0.1),each=10))
  sf2<- apply(sf2,1,sf::st_point,simplify=FALSE)
  sf2<- sf::st_sf(geom=sf::st_sfc(sf2))

  wrongRes<- r2; wrongRes$T1<- 1; wrongRes$T2<- 2; wrongRes$T3<- 3

  expect_equal(as.matrix(.sf_from_raster_list(list(a=single),"t")),
               as.matrix(sf::st_sf(a=rep(1,each=25),t=rep(1,each=25),geometry=rep(sf1,1))))

  expect_equal(as.matrix(.sf_from_raster_list(list(a=good),"t")),
               as.matrix(sf::st_sf(a=rep(1:3,each=25),t=rep(1:3,each=25),geometry=rep(sf1,3))))

  expect_equal(as.matrix(.sf_from_raster_list(list(a=good,b=good),"t")),
               as.matrix(sf::st_sf(a=rep(1:3,each=25),b=rep(1:3,each=25),t=rep(1:3,each=25),geometry=rep(sf1,3))))

  expect_error(.sf_from_raster_list(list(a=good,b=notAllYears)))
  expect_error(.sf_from_raster_list(list(a=good,b=layerX)))
  expect_error(.sf_from_raster_list(list(a=good,b=wrongRes)))
})

test_that("sf to stars",{

})
