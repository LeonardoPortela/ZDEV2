TYPE-POOL zmmr .

TYPES:
  BEGIN OF zmmr_saida,
  rsnum  TYPE resb-rsnum, " Nº reserva
  bwart  TYPE resb-bwart, " Tipo de movimento (administração de estoques)
  kostl  TYPE mseg-kostl, " Centro de custo
  saknr  TYPE resb-saknr, " Nº conta do Razão
  aufnr  TYPE mseg-aufnr, " Nº ordem
  bktxt  TYPE mkpf-bktxt, " Texto de cabeçalho de documento
  emp    TYPE c LENGTH 100, " Denominação da firma ou empresa
  centro TYPE c LENGTH 100, " Centro

  " responsavel pela criação da reserva
  usnam        TYPE rkpf-usnam, " Nome do usuário
  useraliascr  TYPE usrefus-useralias, " Alias para usuário da Internet
  rsdat        TYPE rkpf-rsdat, " Data base da reserva

  " responsavel pela aprovação da reserva
  "uname      TYPE zmmt0003-uname, " Nome do usuário
  uname        TYPE zmmt0009-uname,        " Nome do usuário
  useralias    TYPE usrefus-useralias,     " Alias para usuário da Internet
  dt_aprovacao TYPE zmmt0009-dt_aprovacao, " Data de criação do registro
  hr_aprovacao TYPE zmmt0009-hr_aprovacao, " Hora do registro


  " responsavel pela baixa do estoque
  usnambx     TYPE  mkpf-usnam, " Nome do usuário
  useraliasbx TYPE  usrefus-useralias, " Alias para usuário da Internet
  cpudt       TYPE  mkpf-cpudt, " Data da entrada do documento contábil
  cputm       TYPE  mkpf-cputm, " Hora da entrada

  " Itens
  rspos  TYPE  mseg-rspos, " Item no documento do material
  bdter  TYPE  resb-bdter, " Data necessidade do componente
  matnr  TYPE  mseg-matnr, " Nº do material
  maktx  TYPE  makt-maktx, " Texto breve de material
  erfme  TYPE  mseg-erfme, " Unidade de medida do registro
  lgort  TYPE  mseg-lgort, " Depósito
  bdmng  TYPE  resb-bdmng, " Quantidade necessária
  menge  TYPE  mseg-menge, " Quantidade
  dif    TYPE  mseg-menge, " Diferença
  lgpbe  TYPE  mard-lgpbe, " Posição no depósito
  sgtxt  TYPE  resb-sgtxt, " Texto do item
  erfme2 TYPE  mseg-erfme, " Unidade de medida do registro
  btext  TYPE  t156t-btext," Texto para tipo de movimento (gestão de estoques)
  ltext  TYPE  cskt-ltext, " Descrição
  txt50  TYPE  skat-txt50, " Texto das contas do Razão
  wempf TYPE mseg-wempf,   " Recebedor da mercadoria
END OF zmmr_saida.
