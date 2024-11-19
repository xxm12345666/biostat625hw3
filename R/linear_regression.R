#' Linear Regression Function
#'
#' This function performs a linear regression and calculates data related to linear regression models.
#' @param df A dataframe containing the variables of interest.
#' @param y The dependent variable from the dataframe.
#' @param x The covariates from the dataframe.
#' @return A list containing data related to linear regression models.
#' @export
#' @examples
#' data(mtcars)
#' result=linear_regression_function(mtcars, "mpg", c("cyl","disp","hp"))
linear_regression_function=function(df,y,x){
  #add x1 to x
  y=as.matrix(df[,y,drop=FALSE])
  x=as.matrix(df[,x,drop=FALSE])
  x1=matrix(1,nrow=nrow(df),ncol=1)
  colnames(x1)="Intercept"
  x=cbind(x1,x)

  #calculate
  degree_freedom=nrow(df)-ncol(x)
  beta_hat=solve(t(x)%*%x)%*%t(x)%*%y
  H=x%*%solve(t(x)%*%x)%*%t(x)
  dimension=nrow(H)
  I_matrix=diag(dimension)
  one_matrix=matrix(1,dimension,dimension)
  sigma_hat=sqrt(sum((y-x%*%beta_hat)^2)/degree_freedom)
  var_beta_hat=sigma_hat^2*solve(t(x)%*%x)
  Estimate=beta_hat
  Std_Error=sqrt(diag(var_beta_hat))
  alpha=0.05
  t_q=qt(1-alpha/2,degree_freedom)
  CI_lower=Estimate-t_q*Std_Error
  CI_upper=Estimate+t_q*Std_Error
  SSR=t(y)%*%(H-one_matrix/dimension)%*%y
  SSY=t(y)%*%(I_matrix-one_matrix/dimension)%*%y
  SSE=t(y)%*%(I_matrix-H)%*%y

  R_squared=SSR/SSY
  adjusted_R_squared=1-(SSE/(degree_freedom))/(SSY/(nrow(df)-1))

  t_value=Estimate/Std_Error
  p_value=2*pt(-abs(t_value),degree_freedom)

  F_statistics=(SSR/(ncol(x)-1))/(SSE/degree_freedom)
  p_value_F=1-pf(F_statistics,ncol(x)-1,degree_freedom)

  #create output
  df_output=data.frame(Estimate=Estimate,
                       Std_Error=Std_Error,
                       CI_lower=CI_lower,
                       CI_upper=CI_upper,
                       t_value=t_value,
                       p_value=p_value)
  rownames(df_output)=colnames(x)
  colnames(df_output)=c("Estimate","Std_Error","CI_lower","CI_upper","t_value","p_value")

  residuals_output=y-x%*%beta_hat
  residuals_q=quantile(as.numeric(residuals_output),probs=c(0,0.25,0.5,0.75,1))
  residuals_df=data.frame(Min=residuals_q[1],
                          Q1=residuals_q[2],
                          Median=residuals_q[3],
                          Q3=residuals_q[4],
                          Max=residuals_q[5])

  list_output=list(Residuals=residuals_df,
                   Coefficients=df_output,
                   Residual_standard_error=paste(sigma_hat,"on",degree_freedom,"degrees of freedom"),
                   Multiple_R_squared=as.numeric(R_squared[1,1]),
                   Adjusted_R_squared=as.numeric(adjusted_R_squared[1,1]),
                   F_statistics=paste(F_statistics,"on",ncol(x)-1,"and",degree_freedom,"DF",", p_value:",p_value_F)
  )
  return(list_output)
}
