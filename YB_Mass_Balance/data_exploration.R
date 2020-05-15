test <- total_loads17 %>% 
  filter(!str_detect(SamplingEvent, "Apr 11")) %>% 
  arrange(SamplingEvent) %>% 
  select(-c(LoadUnits, AnalyteGroup, total_inflow)) %>% 
  pivot_wider(names_from = LocType, values_from = total_load) %>%
  rename(below_liberty = "Below Liberty") %>% 
  mutate(
    per_inlet = round(Inlet/below_liberty * 100),
    per_internal = round((below_liberty - Inlet)/below_liberty * 100)
  ) %>% 
  group_nest(Analyte)

bli_umehg <- test %>% slice(4) %>% pull(data) %>% chuck(1) %>% pull(below_liberty)
in_umehg <- test %>% slice(4) %>% pull(data) %>% chuck(1) %>% pull(Inlet)
per_in_umehg <- test %>% slice(4) %>% pull(data) %>% chuck(1) %>% pull(per_inlet)
100 - (mean(in_umehg)/mean(bli_umehg) * 100)
mean(per_in_umehg)
mean(bli_umehg) - mean(in_umehg)

net_loads17 %>% 
  filter(
    Reach == "Entire",
    AnalyteGroup == "MeHg"
  ) %>% 
  select(-c(LoadUnits:Reach, total_inflow)) %>% 
  separate(Analyte, into = c("Analyte", "Fraction"), sep = "- ") %>% 
  pivot_wider(
    id_cols = SamplingEvent,
    names_from = Fraction,
    values_from = net_load
  ) %>% 
  arrange(SamplingEvent)

# percent mehg
perc_mehg <- total_loads17 %>% 
  filter(str_detect(AnalyteGroup, "Hg")) %>% 
  select(-c(LoadUnits, AnalyteGroup, total_inflow)) %>% 
  separate(Analyte, into = c("Analyte", "Fraction"), sep = "- ") %>% 
  pivot_wider(names_from = Analyte, values_from = total_load) %>% 
  mutate(per_mehg = signif(MeHg/THg, 3) * 100) %>% 
  select(-c(MeHg, THg)) %>% 
  arrange(SamplingEvent)

perc_mehg %>% 
  ggplot(aes(x = SamplingEvent, y = per_mehg, fill = LocType)) +
  geom_col(position = "dodge") +
  facet_grid(rows = vars(Fraction)) +
  labs(
    title = "Percent MeHg of Hg",
    x = NULL,
    y = NULL
  ) +
  theme_owhg(x_axis_v = TRUE)

perc_mehg %>% 
  mutate(per_mehg = per_mehg/100) %>% 
  pivot_wider(names_from = Fraction, values_from = per_mehg) %>% 
  arrange(SamplingEvent, LocType) %>% 
  datatable(rownames = FALSE) %>% 
  formatPercentage(3:5, digits = 1)

perc_mehg %>% 
  filter(
    LocType != "Outlet",
    !str_detect(SamplingEvent, "Apr 11")
  ) %>% 
  mutate(per_mehg = per_mehg/100) %>% 
  summ_stat(per_mehg, LocType, Fraction) %>% 
  mutate_at(vars(Mean:IQR), signif, digits = 3) %>% 
  datatable(rownames = FALSE) %>% 
  formatPercentage(4:11, digits = 1)

perc_mehg %>% 
  filter(LocType != "Below Liberty") %>% 
  mutate(per_mehg = per_mehg/100) %>% 
  summ_stat(per_mehg, LocType, Fraction) %>% 
  mutate_at(vars(Mean:IQR), signif, digits = 3) %>% 
  datatable(rownames = FALSE) %>% 
  formatPercentage(4:11, digits = 1)  


net_loads17 %>% 
  filter(
    Reach == "Upper", 
    AnalyteGroup == "MeHg"
  ) %>% 
  pivot_wider(
    id_cols = SamplingEvent,
    names_from = Analyte,
    values_from = net_load
  ) %>%
  rename(
    fMeHg = "MeHg- filtered",
    pMeHg = "MeHg- particulate",
    uMeHg = "MeHg- total"
  ) %>% 
  mutate(
    per_filt = round(fMeHg/uMeHg * 100),
    per_part = round(pMeHg/uMeHg * 100)
  ) %>% 
  arrange(SamplingEvent)


mehg_conc_solids_out <- comb_param_calc %>% 
  filter(
    Year == 2017,
    Parameter == "MeHg Concentration on Solids",
    str_detect(StationName, "^Lib|^Shag|Toe.+[n]$")
  ) %>% 
  select(SampleDate, SamplingEvent, StationName, Value) %>% 
  conv_fact_samplingevent()

mehg_conc_solids_out %>% 
  ggplot(aes(x = SamplingEvent, y = Value)) +
  geom_col() +
  facet_wrap(vars(StationName)) +
  theme_owhg(TRUE)

mehg_conc_out <- conc_data %>% 
  add_num_result() %>% 
  bind_rows(part_conc_calc) %>%
  filter(
    year(SampleDate) == 2017,
    str_detect(Analyte, "^MeHg"),
    str_detect(StationName, "^Lib|^Shag|Toe.+[n]$")
  ) %>% 
  select(StationName, SampleDate, Analyte, Conc)

mehg_conc_out %>% 
  ggplot(aes(x = SampleDate, y = Conc)) +
  geom_point() +
  geom_line() +
  facet_grid(
    cols = vars(StationName),
    rows = vars(Analyte)
  ) +
  theme_owhg()

mehg_conc_out %>% 
  ggplot(aes(x = StationName, y = Conc)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.25) +
  stat_summary( 
    fun = "mean", #add a symbol representing the mean of each group to the plot
    color = "red",
    geom = "point",
    shape = 9, #diamond cross shape
    size = 2 
  ) +
  facet_wrap(vars(Analyte)) +
  theme_owhg(TRUE)

mehg_conc_out %>% 
  summ_stat(Conc, StationName, Analyte) %>% 
  mutate_at(vars(Mean:IQR), signif, digits = 3) %>% 
  datatable(rownames = FALSE)

umehg_conc_out_kw <- mehg_conc_out %>% 
  mutate(StationName = factor(StationName)) %>% 
  group_nest(Analyte) %>% 
  mutate(
    kw_test = map(data, .f = ~with(.x, kruskal.test(Conc ~ StationName))),
    dunn_test = map(data, .f = ~with(.x, FSA::dunnTest(Conc ~ StationName, method = "bonferroni")))
  )

with(umehg_conc_out, kruskal.test(Conc ~ StationName))
with(umehg_conc_out, FSA::dunnTest(Conc ~ StationName, method = "bonferroni"))

net_loads_table <- net_loads %>% 
  arrange(SamplingEvent) %>% 
  group_nest(Reach, AnalyteGroup) %>% 
  mutate(
    load_table = map(
      data,
      .f = pivot_wider,
      id_cols = SamplingEvent,
      names_from = Analyte,
      values_from = net_load
    )
  )

net_loads %>% 
  filter(
    Reach == "Upper", 
    AnalyteGroup == "SuspSolids"
  ) %>% 
  pivot_wider(
    id_cols = SamplingEvent,
    names_from = Analyte,
    values_from = net_load
  ) %>% 
  arrange(SamplingEvent)


source("YB_Mass_Balance/Loads/Import_Net_Load_Data.R")
source("YB_Mass_Balance/Flows/Import_Inlet_Flow_Data_SE.R")

flows_inlet_total17 <- flows_inlet_se %>% 
  filter(Year == 2017) %>% 
  group_by(SamplingEvent) %>% 
  summarize(total_inflow = sum(Flow))

loads_net_umehg17 <- loads_net %>% 
  filter(
    Year == 2017,
    Analyte == "MeHg- total",
    Reach == "Upper"
  ) %>% 
  left_join(flows_inlet_total17) %>% 
  select(Year, net_load, total_inflow)

loads_flow_comb <- loads_flow_cf %>% 
  select(Year, NetLoad, TotalInputFlow) %>% 
  rename(
    net_load = NetLoad,
    total_inflow = TotalInputFlow
  ) %>% 
  bind_rows(loads_net_umehg17)

loads_flow_comb %>% 
  ggplot(aes(x = total_inflow, y = net_load, color = as.character(Year))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

loads_flow_regr <- loads_flow_comb %>% 
  group_nest(Year) %>% 
  mutate(
    lm_model = map(data, .f = ~summary(lm(net_load ~ total_inflow, data = .x)))
  )


