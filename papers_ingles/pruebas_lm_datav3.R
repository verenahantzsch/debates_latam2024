

# pruebas de lm
# no funcan porque tengo años en blanco
# habria que agrupar por periodos

lm1 <-lm(n_debates_año_pais~ncat_eleccion,base_años)
ggplot(base_años, aes(x = ncat_eleccion, y = n_debates_año_pais)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")
summary(lm1)