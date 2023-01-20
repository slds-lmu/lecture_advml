 library(DiceKriging)
 library(mlrMBO)
 configureMlr(show.info = FALSE, show.learner.output = FALSE)
 
  set.seed(1234)
  obj.fun =  makeHimmelblauFunction()
  des = generateDesign(n = 10, par.set = getParamSet(obj.fun), )
  des$y = apply(des, 1, obj.fun)
  surr.km = makeLearner("regr.km", predict.type = "se", covtype = "gauss")
  surr.rf = makeLearner("regr.randomForest", predict.type = "se")
 
  control = makeMBOControl()
  control = setMBOControlTermination(control, iters = 5)
  control = setMBOControlInfill(control, crit = makeMBOInfillCritEI())
 
  run1 = mbo(obj.fun, design = des, learner = surr.km, control = control, show.info = TRUE)
  run2 = mbo(obj.fun, design = des, learner = surr.rf, control = control, show.info = TRUE)
 
 # run = mbo(obj.fun, design = des, learner = surr.km, control = control, show.info = TRUE)
  points = data.frame(run2$opt.path)
 
  x = seq(-4.5, 4.5, length.out = 200)
  z = expand.grid(x1 = x, x2 = x)
  pred1 = predict(run1$models[[1]], newdata = z)
  pred2 = predict(run2$models[[1]], newdata = z)
 
  z$se1 = pred1$data$se
  z$pred1 = pred1$data$response
  z$se2 = pred2$data$se
  z$pred2 = pred2$data$response
 
 
  p1 = ggplot() + ylim(c(-4.5, 4.5)) + xlim(c(-4.5, 4.5))
  p1 = p1 + geom_raster(data = z, aes(x = x1, y = x2, fill = se1), alpha = 0.8)
  p1 = p1 + geom_point(data = points, aes(x = x1, y = x2), size = 3, color = "orange")
  p1 = p1 + labs(fill = expression(s(x))) + theme(legend.position = "bottom")
  p1 = p1 + xlab(expression(lambda[1])) + ylab(expression(lambda[2]))
  p1 = p1 + theme(legend.text = element_text(size = 10))
 
  p2 = ggplot() + ylim(c(-4.5, 4.5)) + xlim(c(-4.5, 4.5))
  p2 = p2 + geom_raster(data = z, aes(x = x1, y = x2, fill = se2), alpha = 0.8)
  p2 = p2 + geom_point(data = points, aes(x = x1, y = x2), size = 3, color = "orange")
  p2 = p2 + labs(fill = expression(s(x))) + theme(legend.position = "bottom")
  p2 = p2 + xlab(expression(lambda[1])) + ylab(expression(lambda[2]))
  p2 = p2 + theme(legend.text = element_text(size = 10))
 
  grid.arrange(p1, p2, ncol = 2)
@


