AllData


first_author_income_cats_jrnls<-AllData %>% 
  filter(AuthorNum==1) %>% 
  group_by(pair_key,Journal,JrnlType,IncomeGroup) %>% 
  tally() %>% 
  mutate(percentage=n/sum(n)*100)

first_author_income_cats_jrnls<-first_author_income_cats_jrnls %>% filter(pair_key=="1"|pair_key=="2"|pair_key=="3")

labels <- c(OA = "Open Access Articles", PW = "Paywalled Articles")
plot1.1<-ggplot(first_author_income_cats_jrnls, aes(x=IncomeGroup,y = percentage))+
  geom_bar(stat = "identity")+
  xlab("1st Author National Income Category") + ylab("Percent")+
  # scale_x_discrete(limits = bar_order)+
  facet_grid(cols = vars(JrnlType),rows = vars(pair_key),labeller=labeller(JrnlType = labels))+
  ggtitle("Figure 1")
plot1.1<-plot1.1+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
plot1.1