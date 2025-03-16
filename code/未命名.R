#the modeling construction and interperation.



# Ensure policy views are ordinal factors
US_election$Y_Tax <- factor(US_election$tax_on_millionaries, ordered = TRUE)
US_election$Y_LGBTQ <- factor(US_election$Lgbgt_job_discrimination, ordered = TRUE)
US_election$Y_Immigration <- factor(US_election$illegel_immiggration, ordered = TRUE)


# Stage 1: Race only (Ordinal Logistic Regression)
model1_stage1_tax <- polr(Y_Tax ~ race, data = US_election, weights = weight, Hess = TRUE)

# Stage 2: Adding Income and Controls
model1_stage2_tax <- polr(Y_Tax ~ race + income + religion + transgender_policy + edu_summary + party_hand_tax + religious_binary, 
                          data = US_election, weights = weight, Hess = TRUE)




model1_stage1_lgbtq <- polr(Y_LGBTQ ~ race, data = US_election, weights = weight, Hess = TRUE)
model1_stage1_immigration <- polr(Y_Immigration ~ race, data = US_election, weights = weight, Hess = TRUE)

# Stage 2: Adding Income and Controls
model1_stage2_tax <- polr(Y_Tax ~ race + income + religion + transgender_policy + edu_summary + party_hand_tax + religious_binary, 
                          data = US_election, weights = weight, Hess = TRUE)

model1_stage2_lgbtq <- polr(Y_LGBTQ ~ race + income + religion + transgender_policy + edu_summary + party_hand_tax + religious_binary, 
                            data = US_election, weights = weight, Hess = TRUE)

model1_stage2_immigration <- polr(Y_Immigration ~ race + income + religion + transgender_policy + edu_summary + party_hand_tax + religious_binary, 
                                  data = US_election, weights = weight, Hess = TRUE)

# Summary of results
summary(model1_stage1_tax)
summary(model1_stage1_lgbtq)
summary(model1_stage1_immigration)
summary(model1_stage2_tax)
summary(model1_stage2_lgbtq)
summary(model1_stage2_immigration)


```
