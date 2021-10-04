# Global view
desc_table(dep_vars = c("index_knowledge", "policies_support > 0", "policies_fair > 0", "standard_support > 0", "standard_public_transport_support > 0", "investments_support > 0", "tax_transfers_support > 0"), filename = "Heterogeneity/policies_het_female_all",
           dep.var.labels = c("Knowledge Index", "Support policies", "Policies are fair", "Support \n ban on combustion engine", "Support \n ban on combustion engine with alternative", "Support \n Green infrastructure program", "Support \n Carbon tax with cash transfers"),
           nolabel = F,
           dep.var.caption = c(""), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
)

## Responsible party for CC
desc_table(dep_vars = c("responsible_CC_each > 0", "responsible_CC_rich > 0", "responsible_CC_govt > 0", "responsible_CC_companies > 0", "responsible_CC_past > 0"), filename = "Heterogeneity/responsible_GHG_het_female_all",
           dep.var.labels = c("Each of us","The rich",  "Governments", "Companies", "Previous generations"),
           nolabel = F,
           dep.var.caption = c("Predominantly responsible for CC…"), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
)


## Possible to halt CC
desc_table(dep_vars = c("net_zero_feasible >= 1", "CC_affects_self >= 1", "CC_will_end >= 1", "effect_halt_CC_economy >= 1", "effect_halt_CC_lifestyle >= 1"), filename = "Heterogeneity/CC_stoppable_het_female_all",
           dep.var.labels = c("Technically feasible","Affected personally",  "Halt by end of century", "Positive effects on the economy", "Negative effects personally"),
           dep.var.caption = c(""), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
)


## Willing to change lifestyle
desc_table(dep_vars = c("willing_limit_flying >= 1", "willing_limit_driving >= 1", "willing_electric_car >= 1", "willing_limit_beef >= 1", "willing_limit_heating >= 1"), filename = "Heterogeneity/limit_behavior_het_female_all",
           dep.var.labels = c("Limit flying","Limit driving",  "Have an eletric vehicle", "Limit beef consumption", "Limit heating"),
           dep.var.caption = c("Willing to change lifestyle?"), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
)
## Conditions
desc_table(dep_vars = c("condition_ambitious_policies >= 1", "condition_financial_aid >= 1", "condition_people_change >= 1", "condition_rich_change >= 1"), filename = "Heterogeneity/change_condition_het_female_all",
           dep.var.labels = c("Ambitious policies", "Financial support", "People around changing", "Rich changing"),
           dep.var.caption = c("Important factors"), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
)

# funding
desc_table(dep_vars = c("investments_funding_debt == 1", "investments_funding_sales_tax == 1", "investments_funding_wealth_tax == 1", "investments_funding_less_social == 1", "investments_funding_less_military == 1"), filename = "Heterogeneity/investments_funding_het_female_all",
           dep.var.labels = c("Public debt", "Sales tax", "Wealth tax", "Reduce social spending", "Reduce military spending"),
           dep.var.caption = c("Appropriate source of funding"), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
           )

# Support policies climate
desc_table(dep_vars = c("policy_tax_flying > 0", "policy_tax_fuels > 0", "policy_ban_city_centers > 0", 
                        "policy_subsidies > 0", "policy_climate_fund > 0"), filename = "Heterogeneity/policy_climate_het_female_all",
           dep.var.labels = c("Tax on flying", "Tax on fossil fuels", "Ban polluting vehicles in city centers", "Technology subsidies", "Global climate fund"),
           dep.var.caption = c("Support"), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
           )

# Revenues of carbon tax
desc_table(dep_vars = c("tax_transfer_constrained_hh > 0", "tax_transfer_poor > 0", "tax_transfer_all > 0", "tax_rebates_affected_firms > 0", 
                        "tax_reduction_corporate_tax > 0", "tax_reduction_personal_tax > 0", "tax_investments > 0", "tax_subsidies > 0", "tax_reduction_deficit > 0"), filename = "Heterogeneity/revenue_carbon_tax_het_female_all",
           dep.var.labels = c("Transfer to constrained HH", "Transfers to poorest", "Equal transfers", "Tax rebates for affected firms", "Reduce CIT", "Reduce PIT", "Infrastructure projects", "Technology subsidies", "Reduce deficit"),
           dep.var.caption = c("Support carbon tax if revenues allocated as/to…"), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
           )

## WTP
desc_table(dep_vars = c("wtp == 'Yes'"), filename = "Heterogeneity/wtp_het_female_all",
           dep.var.labels = c("WTP"),
           nolabel=F,
           dep.var.caption = c("WTP to limit global warming to safe levels"), data = e, indep_vars = c(control_variables, "as.factor(wtp_variant)", "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
           )

## Donation/Petition
desc_table(dep_vars = c("donation", "petition == 'Yes'"), filename = "Heterogeneity/altruism_het_female_all",
           dep.var.labels = c("Donation to charity \\textdollar", "Signed petition"),
           dep.var.caption = c("Altruism"), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
           )

# Level for PP to tackle CC
desc_table(dep_vars = c("scale_local", "scale_state", "scale_federal", "scale_global"), filename = "Heterogeneity/scale_het_female_all",
           dep.var.labels = c("Local","State", "Federal", "Global"),
           dep.var.caption = c("Policy level"), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
)
# US should act
desc_table(dep_vars = c("should_fight_CC > 0", "if_other_do_more > 0", "if_other_do_less > 0"), filename = "Heterogeneity/country_should_act_condition_het_female_all",
           dep.var.labels = c("fight climate change","be more ambitious, if others more", "be more ambitious, if others less"),
           dep.var.caption = c("U.S. should… "), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
  )

# Burden
desc_table(dep_vars = c("burden_sharing_income >= 1", "burden_sharing_emissions >= 1", "burden_sharing_cumulative >= 1", "burden_sharing_rich_pay >= 1", "burden_sharing_poor_receive >= 1"), filename = "Heterogeneity/burden_sharing_het_female_all",
           dep.var.labels = c("Pay in proportion to income","Pay in proportion to current emissions", "Pay in proportion to past emissions (from 1990)", "Richest pay alone", "Richest pay, and even more to help vulnerable countries"),
           dep.var.caption = c("Countries should"), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
)
# Approve propositions
desc_table(dep_vars = c("global_assembly_support > 0", "global_tax_support > 0", "tax_1p_support > 0"), filename = "Heterogeneity/support_inter_het_female_all",
           dep.var.labels = c("Global democratic assembly to fight CC", "Global tax on GHG emissions funding a global basic income (\\textdollar 30/month/adult)", "Global tax on top 1\\% to finance poorest countries"),
           dep.var.caption = c("Approve"), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
)

desc_table(dep_vars = c("will_insulate > 0"), filename = "Heterogeneity/will_insulate_het_female_all",
           dep.var.labels = c("Likely to insulate"),
           dep.var.caption = c(""), data = e[e$home_owner == T,], indep_vars = c(control_variables,"obstacles_insulation_cannot", "obstacles_insulation_cost", "obstacles_insulation_effort", "obstacles_insulation_useless", "obstacles_insulation_satisfactory", "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
           )

desc_table(dep_vars = c("insulation_support > 0"), filename = "Heterogeneity/pref_thermal_het_female_all",
           dep.var.labels = c("Support thermal renovation if subsidized"),
           dep.var.caption = c(""), data = e, indep_vars = c(control_variables, "insulation_disruption_variant" ,"urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
           )

desc_table(dep_vars = c("beef_tax_support > 0", "beef_subsidies_vegetables_support > 0", "beef_subsidies_removal_support > 0", "beef_ban_intensive_support > 0"), filename = "Heterogeneity/pref_beef_het_female_all",
           dep.var.labels = c("Tax on cattle products (beefx2)", "Subsidies Vegetables", "No subsidies cattle", "Ban intensive cattle"),
           dep.var.caption = c("If gov. limits cattle products, I would support…"), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
           )

# Trust
desc_table(dep_vars = c("can_trust_people > 0", "can_trust_govt > 0"), filename = "Heterogeneity/trust_het_female_all",
           dep.var.labels = c("most people","government to do what is right"),
           dep.var.caption = c("Trust…"), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
          )
# Positive views
desc_table(dep_vars = c("view_govt > 0", "problem_inequality > 0", "future_richness <= 0"), filename = "Heterogeneity/ineq_intervention_future_het_female_all",
           dep.var.labels = c("Active government","Inequality serious problem", "World poorer or same"),
           dep.var.caption = c(""), data = e, indep_vars = c(control_variables, "urban", "treatment*female"), keep = c("treatment", "female"), indep_labels = c("Female", "Treatment Climate", "Treatment Policy", "Treatment Both", "Female $\times$ Treatment Climate", "Female $\times$ Treatment Policy", "Female $\times$ Treatment Both"), mean_control = T
)



