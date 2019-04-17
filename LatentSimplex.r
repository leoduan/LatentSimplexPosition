require("tensorflow")

#input:
# S: an n x n similarity matrix
# G: number of clusters
# lambda: the amount of L_21 regularization (can be set to 0)
SimplexClust<- function(S, G=2, lambda = 1, eps= 1E-2,steps=2000, choice_loss = "KL"){

	N<- nrow(S)
	# create a simplex matrix
	P0 <-  tf$abs(tf$Variable(tf$random_uniform(shape(N,G), 0, 1)))
	P0sum<- tf$reduce_sum(P0,  axis=as.integer(1), keepdims=TRUE)

	# zero out the diagonal matrix in the A matrix
	ZeroDiagInd<- matrix(1,N,N)
	diag(ZeroDiagInd)<- 0
	ZeroDiagIndTF <- tf$cast(ZeroDiagInd, tf$float32)
	P = P0 / P0sum
	A = tf$matmul(P, P,transpose_b = T)

	S_tf = tf$cast(S,tf$float32)
	logS_tf = tf$log(S_tf)

	if(choice_loss == "KL"){
		# #KL loss: S||A
		logRatio= - tf$log(A)
		logRatioComp=  - tf$log(1.0-A)
		loss_mat = tf$multiply( S_tf , logRatio ) + tf$multiply( 1.0- S_tf, logRatioComp)
	}

	if(choice_loss == "L2"){
		## L2 loss
		loss_mat = tf$multiply(S_tf - A,S_tf - A)
	}

	loss = tf$reduce_sum(loss_mat * ZeroDiagIndTF)

	L21_norm = tf$reduce_sum(tf$sqrt(tf$reduce_sum(P*P, as.integer(0))))

	rank_prior_loss =  L21_norm

	relax_loss =  1E3 * tf$reduce_sum(tf$multiply(P0sum-1,P0sum-1))

	total_loss = loss +  lambda * rank_prior_loss  + relax_loss

	optimizer <- tf$train$AdamOptimizer(eps)

	train <- optimizer$minimize(total_loss)

	# Launch the graph and initialize the variables.
	sess = tf$Session()
	sess$run(tf$global_variables_initializer())

	for (step in 1:steps) {
		sess$run(train)
		if (step %% 20 == 0)
			cat(step, "-", sess$run(loss)," ", sess$run(rank_prior_loss),"\n")
	}

	est_P<- sess$run(P)
	est_A<- sess$run(A)

	return(list("P"=est_P, "A"=est_A))
}
