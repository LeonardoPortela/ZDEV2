class ZCL_ROTA definition
  public
  create public .

*"* public components of class ZCL_ROTA
*"* do not include other source files here!!!
public section.

  methods CONSTRUCTOR .
  methods SET_ID_ROTA
    importing
      !I_ID_ROTA type CHAR13 .
  methods SET_CAT_VEICULO
    importing
      !I_CAT_VEICULO type NUMC2 .
  methods SET_DT_VIGENCIA
    importing
      !I_DT_VIGENCIA type SYDATUM .
  methods SET_MUNIC_ORIGEM
    importing
      !I_MUNIC_ORIGEM type ZMUNIC_IBGE .
  methods SET_MUNIC_DESTINO
    importing
      !I_MUNIC_DESTINO type ZMUNIC_IBGE .
  methods SET_DISTANCIA
    importing
      !I_DISTANCIA type DISTZ .
  methods SET_VLR_PEDAGIO
    importing
      !I_VLR_PEDAGIO type NETPR .
  methods SET_DESCR_ROTA
    importing
      !I_DESCR_ROTA type CHAR255 .
  methods SET_BUKRS
    importing
      !I_BUKRS type BUKRS .
  methods SET_BRANCH
    importing
      !I_BRANCH type J_1BBRANC_ .
  methods GET_ID_ROTA
    returning
      value(E_ID_ROTA) type CHAR13 .
  methods GET_CAT_VEICULO
    returning
      value(E_CAT_VEICULO) type CHAR2 .
  methods GET_DT_VIGENCIA
    returning
      value(E_DT_VIGENCIA) type SYDATUM .
  methods GET_MUNIC_ORIGEM
    returning
      value(E_MUNIC_ORIGEM) type ZMUNIC_IBGE .
  methods GET_MUNIC_DESTINO
    returning
      value(E_MUNIC_DESTINO) type ZMUNIC_IBGE .
  methods GET_DISTANCIA
    returning
      value(E_DISTANCIA) type DISTZ .
  methods GET_VLR_PEDAGIO
    returning
      value(E_VLR_PEDAGIO) type NETPR .
  methods GET_DESCR_ROTA
    returning
      value(E_DESCR_ROTA) type CHAR255 .
  methods GET_BUKRS
    returning
      value(R_BUKRS) type BUKRS .
  methods GET_BRANCH
    returning
      value(R_BRANCH) type J_1BBRANC_ .
protected section.
*"* protected components of class ZCL_ROTA
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_ROTA
*"* do not include other source files here!!!

  data AT_ID_ROTA type CHAR13 .
  data AT_CAT_VEICULO type CHAR2 .
  data AT_DT_VIGENCIA type SYDATUM .
  data AT_MUNIC_ORIGEM type ZMUNIC_IBGE .
  data AT_MUNIC_DESTINO type ZMUNIC_IBGE .
  data AT_DISTANCIA type DISTZ .
  data AT_VLR_PEDAGIO type NETPR .
  data AT_DESCR_ROTA type STRING .
  data AT_BUKRS type BUKRS .
  data AT_BRANCH type J_1BBRANC_ .
ENDCLASS.



CLASS ZCL_ROTA IMPLEMENTATION.


METHOD CONSTRUCTOR.

  "Limpar todas os atributos da classe quando ela estiver sendo instanciada.
  CLEAR:
        ME->AT_ID_ROTA,
        ME->AT_CAT_VEICULO,
        ME->AT_DT_VIGENCIA,
        ME->AT_MUNIC_ORIGEM,
        ME->AT_MUNIC_DESTINO,
        ME->AT_DISTANCIA,
        ME->AT_VLR_PEDAGIO,
        ME->AT_DESCR_ROTA.

ENDMETHOD.


  method GET_BRANCH.
    R_BRANCH = ME->AT_BRANCH.
  endmethod.


  method GET_BUKRS.
    R_BUKRS = ME->AT_BUKRS.
  endmethod.


METHOD GET_CAT_VEICULO.
************************************
*  Método de Acesso
*  Atributo: AT_CAT_VEICULO
*  Parâmetro:
*  Retorno:
*  Descrição: Método responsavel por retornar a categoria da rota.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 13:21:36
************************************
  E_CAT_VEICULO = ME->AT_CAT_VEICULO.
ENDMETHOD.


method GET_DESCR_ROTA.
************************************
*  Método de Acesso
*  Atributo: AT_DESCR_ROTA
*  Parâmetro:
*  Retorno: E_DESCR_ROTA
*  Descrição: Método responsavel por retornar a descrição da rota.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 13:26:41
************************************
  E_DESCR_ROTA = ME->AT_DESCR_ROTA.
endmethod.


method GET_DISTANCIA.
************************************
*  Método de Acesso
*  Atributo: AT_DISTANCIA
*  Parâmetro:
*  Retorno: E_DISTANCIA
*  Descrição: Método responsavel por retonrar a distancia da rota.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 13:25:13
************************************
  E_DISTANCIA = ME->AT_DISTANCIA.
endmethod.


METHOD GET_DT_VIGENCIA.
************************************
*  Método de Acesso
*  Atributo: AT_DT_VIGENCIA
*  Parâmetro:
*  Retorno: E_DT_VIGENCIA
*  Descrição: Método responsavel por retonar a Data de Vigencia.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 13:22:22
************************************
  E_DT_VIGENCIA = ME->AT_DT_VIGENCIA.
ENDMETHOD.


method GET_ID_ROTA.
************************************
*  Método de Acesso
*  Atributo: AT_ID_ROTA
*  Parâmetro:
*  Retorno: E_ID_ROTA
*  Descrição: Método responsavel por retornar o ID da Rota.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 13:20:45
************************************
    E_ID_ROTA = ME->AT_ID_ROTA.
endmethod.


method GET_MUNIC_DESTINO.
************************************
*  Método de Acesso
*  Atributo: AT_MUNIC_DESTINO
*  Parâmetro:
*  Retorno: E_MUNIC_DESTINO
*  Descrição: Método responsavel por retornar o código do municipio de destino.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 13:24:16
************************************
  E_MUNIC_DESTINO = ME->AT_MUNIC_DESTINO.
endmethod.


METHOD GET_MUNIC_ORIGEM.
************************************
*  Método de Acesso
*  Atributo: AT_MUNIC_ORIGEM
*  Parâmetro:
*  Retorno: E_MUNIC_ORIGEM
*  Descrição: Método responsavel por retonar o código do municipio origem.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 13:23:23
************************************
  E_MUNIC_ORIGEM = ME->AT_MUNIC_ORIGEM.
ENDMETHOD.


method GET_VLR_PEDAGIO.
************************************
*  Método de Acesso
*  Atributo: AT_VLR_PEDAGIO
*  Parâmetro:
*  Retorno: E_VLR_PEDAGIO
*  Descrição: Método responsavel para retornar o valor do pedagio.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 13:25:57
************************************
  E_VLR_PEDAGIO = ME->AT_VLR_PEDAGIO.
endmethod.


  METHOD SET_BRANCH.
    ME->AT_BRANCH = I_BRANCH.
  ENDMETHOD.


  method SET_BUKRS.
    ME->AT_BUKRS = I_BUKRS.
  endmethod.


METHOD SET_CAT_VEICULO.
************************************
*  Método de Configuração
*  Atributo: AT_CAT_VEICULO
*  Parâmetro: I_CAT_VEICULO
*  Descrição: Método responsavel por atribuir a categoria do veiculo ao objeto.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 11:14:34
************************************
  ME->AT_CAT_VEICULO = I_CAT_VEICULO.
ENDMETHOD.


METHOD SET_DESCR_ROTA.
************************************
*  Método de Configuração
*  Atributo:  AT_DESCR_ROTA
*  Parâmetro: I_DESR_ROTA
*  Descrição: Método responsavel por atribuir a descrição da rota ao objeto.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 11:27:57
************************************
  ME->AT_DESCR_ROTA = I_DESCR_ROTA.
ENDMETHOD.


METHOD SET_DISTANCIA.
************************************
*  Método de Configuração
*  Atributo: AT_DISTANCIA
*  Parâmetro: I_DISTANCIA
*  Descrição: Método responsavel por atribuir a distancia ao objeto.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 11:20:47
************************************
  ME->AT_DISTANCIA = I_DISTANCIA.
ENDMETHOD.


METHOD SET_DT_VIGENCIA.
************************************
*  Método de Configuração
*  Atributo: AT_DT_VIGENCIA
*  Parâmetro: I_DT_VIGENCIA
*  Descrição: Método responsavel por atribuir a data de vigencia ao objeto.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 11:15:21
************************************

  DATA: ZCL_UTIL TYPE REF TO ZCL_UTIL,
        DATA     TYPE C LENGTH 10.

  CLEAR: DATA.

  CREATE OBJECT ZCL_UTIL.

  DATA = I_DT_VIGENCIA.

  ME->AT_DT_VIGENCIA = ZCL_UTIL->CONV_DATA_BR_US( DATA ).

ENDMETHOD.


METHOD SET_ID_ROTA.
************************************
*  Método de Configuração
*  Atributo: AT_ID_ROTA
*  Parâmetro: I_ID_ROTA
*  Descrição: Método responsavel por atribuir ao objeto o ID da Rota.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 11:13:08
************************************
  ME->AT_ID_ROTA = I_ID_ROTA.
ENDMETHOD.


METHOD SET_MUNIC_DESTINO.
************************************
*  Método de Configuração
*  Atributo: AT_MUNIC_DESTINO
*  Parâmetro: I_MUNIC_DESTINO
*  Descrição: Método responsavel por atribuir o código do municipio de destino ao objeto.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 11:18:35
************************************
  ME->AT_MUNIC_DESTINO = I_MUNIC_DESTINO.
ENDMETHOD.


METHOD SET_MUNIC_ORIGEM.
************************************
*  Método de Configuração
*  Atributo: AT_MUNIC_ORIGEM
*  Parâmetro: I_MUNIC_ORIGEM
*  Descrição: Método responsavel por atribuir o código do municipio origem ao objeto.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 11:17:32
************************************
  ME->AT_MUNIC_ORIGEM = I_MUNIC_ORIGEM .
ENDMETHOD.


METHOD SET_VLR_PEDAGIO.
************************************
*  Método de Configuração
*  Atributo: AT_VLR_PEDAGIO
*  Parâmetro: I_VLR_PEDAGIO
*  Descrição: Método responsavel por atribuir o valor do pedagio ao objeto.
*  Developer: Victor Hugo Souza Nunes
*  11.04.2014 11:25:22
************************************
  ME->AT_VLR_PEDAGIO = I_VLR_PEDAGIO.
ENDMETHOD.
ENDCLASS.
