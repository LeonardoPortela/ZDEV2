interface ZIF_USER_PERFIL
  public .


  data AT_USER type UNAME .
  class-data AT_USER_PERFIL type ref to ZIF_USER_PERFIL .
  data AT_LOGONDATA type BAPILOGOND .
  data AT_DEFAULTS type BAPIDEFAUL .
  data AT_ADDRESS type BAPIADDR3 .
  data AT_COMPANY type BAPIUSCOMP .
  data AT_SNC type BAPISNCU .
  data AT_REF_USER type BAPIREFUS .
  data AT_ALIAS type BAPIALIAS .
  data AT_UCLASS type BAPIUCLASS .
  data AT_LASTMODIFIED type BAPIMODDAT .
  data AT_ISLOCKED type BAPISLOCKD .
  data AT_IDENTITY type BAPIIDENTITY .
  data AT_ADMINDATA type BAPIUSERADMIN .
  data AT_DESCRIPTION type BAPIUSDESC .
  data AT_PARAMETER type RSSBR_T_BADI_PARAMETER .
  data AT_PROFILES type RSSBR_T_BADI_BAPIPROF .
  data AT_ACTIVITYGROUPS type RSSBR_T_BADI_BAPIAGR .
  data AT_ADDTEL type ZRCF_T_BAPIADTEL .
  data AT_ADDFAX type zRCF_T_BAPIADFAX .
  data AT_ADDTTX type RSCRMADTTX_T .
  data AT_ADDTLX type RSCRMADTLX_T .
  data AT_ADDSMTP type RSCRMADSMTP_T .
  data AT_ADDRML type RSCRMADRML_T .
  data AT_ADDX400 type RSCRMADX400_T .
  data AT_ADDRFC type RSCRMADRFC_T .
  data AT_ADDPRT type RSCRMADPRT_T .
  data AT_ADDSSF type RSCRMADSSF_T .
  data AT_ADDURI type RSCRMADURI_T .
  data AT_ADDPAG type RSCRMADPAG_T .
  data AT_ADDCOMREM type RSCRMCOMREM_T .
  data AT_PARAMETER1 type SUID_TT_BAPIPARAM1 .
  data AT_GROUPS type RSSBR_T_BADI_BAPIGROUPS .
  data AT_UCLASSSYS type SUID_TT_BAPIUCLASSSYS .
  data AT_SYSTEMS type SUID_TT_BAPIRCVSYS .

  class-methods GERA_ERRO_GERAL
    importing
      !I_TEXTO type STRING
    raising
      ZCX_USER_PERFIL .
  class-methods GET_INSTANCE
    returning
      value(R_USER_PERFIL) type ref to ZIF_USER_PERFIL
    raising
      ZCX_USER_PERFIL .
  methods SET_ATRIBUIR_FUNCOES
    importing
      !I_FUNCAO type BAPIAGR
    returning
      value(R_USER_PERFIL) type ref to ZIF_USER_PERFIL
    raising
      ZCX_USER_PERFIL .
  methods SET_ATRIBUIR_FUNCOES_DELETE
    importing
      !I_FUNCAO type BAPIAGR
    returning
      value(R_USER_PERFIL) type ref to ZIF_USER_PERFIL
    raising
      ZCX_USER_PERFIL .
  methods SET_USUARIO
    importing
      !I_USUARIO type UNAME
    returning
      value(R_USER_PERFIL) type ref to ZIF_USER_PERFIL .
  methods SET_LIMPAR
    returning
      value(R_USER_PERFIL) type ref to ZIF_USER_PERFIL .
endinterface.
