-- Exercicio 2
dobro::Float->Float
dobro x = x * 2
disEspaco::Float->Float
disEspaco[[x,y,z],[x1,y1,z1]] = sqrt((dobro x1 - x) + (dobro y1 - y) + (dobro z1 - z))