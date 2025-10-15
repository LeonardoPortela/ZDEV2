*&---------------------------------------------------------------------*
*Report  ZFIR052
*TITULO  : Tax Reports - VAT
*AUTOR   : ANTONIO LUIZ RODRIGUES DA SILVA
*DATA.   : 09.02.2015
*TRANSACAO: ZFI0070
*&---------------------------------------------------------------------*
report  ZFIR052.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
type-pools: ICON,
            SLIS.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
tables: ZIB_CONTABIL, BKPF.


*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*


types: begin of TY_ESTRUTURA.
         include type SLIS_FIELDCAT_MAIN.
         include type SLIS_FIELDCAT_ALV_SPEC.
types: end of TY_ESTRUTURA.

types:
  begin of TY_BSAS,
    BUKRS  type BSAS-BUKRS,
    BUDAT  type BSAS-BUDAT,
    BLDAT  type BSAS-BLDAT,
    HKONT  type BSAS-HKONT,
    GJAHR  type BSAS-GJAHR,
    BELNR  type BSAS-BELNR,
    WAERS  type BSAS-WAERS,
    STBLG  type BKPF-STBLG,
    STJAH  type BKPF-STJAH,
    DMBTR  type BSAS-DMBTR,
    DMBE2  type BSAS-DMBE2,
    SHKZG  type BSAS-SHKZG,
    BLART  type BSAS-BLART,
    BSCHL  type BSAS-BSCHL,
    SGTXT  type BSAS-SGTXT,
    DEL(1),
  end of TY_BSAS,

  begin of TY_BSET,
    BUKRS type BSET-BUKRS,
    BELNR type BSET-BELNR,
    GJAHR type BSET-GJAHR,
    HWBAS type BSET-HWBAS,
    FWBAS type BSET-FWBAS,
    HWSTE type BSET-HWSTE,
    HKONT type BSET-HKONT,
    SHKZG type BSET-SHKZG,
    LSTML type BSET-LSTML,
    MWSKZ type BSET-MWSKZ,
  end of TY_BSET,

  begin of TY_BSAK,
    BUKRS type BSAK-BUKRS,
    BELNR type BSAK-BELNR,
    GJAHR type BSAK-GJAHR,
    LIFNR type BSAK-LIFNR,
    XBLNR type BSAK-XBLNR,
  end of TY_BSAK,

  begin of TY_BSAD,
    BUKRS type BSAD-BUKRS,
    BELNR type BSAD-BELNR,
    GJAHR type BSAD-GJAHR,
    KUNNR type BSAD-KUNNR,
    XBLNR type BSAD-XBLNR,
  end of TY_BSAD,

  begin of TY_ZIB,
    OBJ_KEY      type ZIB_CONTABIL-OBJ_KEY,
    TAX_CODE     type ZIB_CONTABIL-TAX_CODE,
    LAND1        type ZIB_CONTABIL-LAND1,
    HKONT        type ZIB_CONTABIL-HKONT,
    BSCHL        type ZIB_CONTABIL-BSCHL,
    BUDAT        type ZIB_CONTABIL-BUDAT,
    BLDAT        type ZIB_CONTABIL-BLDAT,
    ZUONR        type ZIB_CONTABIL-ZUONR,
    XBLNR        type ZIB_CONTABIL-XBLNR,
    BUKRS        type ZIB_CONTABIL-BUKRS,
    WAERS        type ZIB_CONTABIL-WAERS,
    CONTROLE_VAT type ZIB_CONTABIL-CONTROLE_VAT,
    SEQITEM      type ZIB_CONTABIL-SEQITEM,
    WRBTR        type ZIB_CONTABIL-WRBTR,
    DMBE2        type ZIB_CONTABIL-DMBE2,
    KIDNO        type ZIB_CONTABIL-KIDNO,
    SGTXT        type ZIB_CONTABIL-SGTXT,
    STBLG        type BKPF-STBLG,
    STJAH        type BKPF-STJAH,
    DEL(1),
  end of TY_ZIB,

  begin of TY_BSIS,
    BUKRS type BSIS-BUKRS,
    BELNR type BSIS-BELNR,
    GJAHR type BSIS-GJAHR,
    BUZEI type BSIS-BUZEI,
    DMBTR type BSIS-DMBTR,
  end of TY_BSIS,

  begin of TY_KNA1,
    KUNNR type KNA1-KUNNR,
    NAME1 type KNA1-NAME1,
    LAND1 type KNA1-LAND1,
  end of TY_KNA1,

  begin of TY_SKAT,
    SAKNR type SKAT-SAKNR,
    TXT50 type SKAT-TXT50,
  end of TY_SKAT,

  begin of TY_T005T,
    LAND1 type T005T-LAND1,
    LANDX type T005T-LANDX,
  end of TY_T005T,

  begin of TY_SAIDA,
    LAND1      type ZIB_CONTABIL-LAND1,
    LANDX      type T005T-LANDX,
    TAX_CODE   type ZIB_CONTABIL-KIDNO,
    KOART      type TBSL-KOART,
    TYPE(20),
    VAT_BASIS  type ZIB_CONTABIL-WRBTR,
    VAT        type ZIB_CONTABIL-WRBTR,
    RATE       type KURSF,
    DMBE2      type ZIB_CONTABIL-DMBE2,
    VATP       type ZIB_CONTABIL-WRBTR,
    VAT_BASISD type ZIB_CONTABIL-WRBTR,
    VATD       type ZIB_CONTABIL-WRBTR,
    QTDE       type I,
  end of TY_SAIDA,

  begin of TY_SAIDA_D,
    BUKRS      type ZIB_CONTABIL-BUKRS,
    HKONT      type ZIB_CONTABIL-HKONT,
    NAME1      type KNA1-NAME1,
    BELNR      type ZIB_CONTABIL_CHV-BELNR,
    STBLG      type BKPF-STBLG,
    BUDAT      type ZIB_CONTABIL-BUDAT,
    BLDAT      type ZIB_CONTABIL-BLDAT,
    ZUONR      type ZIB_CONTABIL-ZUONR,
    XBLNR      type ZIB_CONTABIL-XBLNR,
    TAX_CODE   type ZIB_CONTABIL-TAX_CODE,
    LANDX      type T005T-LANDX,
    TYPE(20),
    VAT_BASIS  type ZIB_CONTABIL-WRBTR,
    VAT        type ZIB_CONTABIL-WRBTR,
    VAT_BASISD type ZIB_CONTABIL-WRBTR,
    VATD       type ZIB_CONTABIL-WRBTR,
    RATE       type KURSF,
    VATP       type ZIB_CONTABIL-WRBTR,
    HKONTV     type ZIB_CONTABIL-HKONT,
    TXT50      type SKAT-TXT50,
    SGTXT      type ZIB_CONTABIL-SGTXT,
    ORIGEM(20),
  end of TY_SAIDA_D.


*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*

data: IT_SAIDA            type table of TY_SAIDA,
      IT_SAIDA_D          type table of TY_SAIDA_D,
      IT_TBSL             type table of TBSL,
      IT_TBSL_SAP         type table of TBSL,
      IT_KNA1             type table of TY_KNA1,
      IT_LFA1             type table of TY_KNA1,
      IT_BSIS             type table of TY_BSIS,
      IT_BSAS             type table of TY_BSAS,
      IT_BSAS_AUX         type table of TY_BSAS,
      IT_BSET             type table of TY_BSET,
      IT_BSET_AUX         type table of TY_BSET,
      IT_BSAK             type table of TY_BSAK,
      IT_BSAD             type table of TY_BSAD,
      IT_T005T            type table of TY_T005T,
      IT_T005             type table of T005,
      IT_SKAT             type table of TY_SKAT,
      IT_ZIB_CONTABIL_AUX type table of TY_ZIB,
      TL_T007S            type table of T007S,
      IT_ZIB_CONTABIL     type table of ZIB_CONTABIL,
      IT_ZIB_CONTABIL_F   type table of ZIB_CONTABIL,
      IT_ZIB_CONTABIL_A0  type table of TY_ZIB,
      IT_ZIB_CONTABIL_CHV type table of ZIB_CONTABIL_CHV,
      T_VAT201            type standard table of  RGSB4 with header line,
      T_SET               type standard table of SETLEAF  with header line,
      T_LAY               type standard table of SETLINET with header line.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*

data: WA_SAIDA            type TY_SAIDA,
      WA_SAIDA_D          type TY_SAIDA_D,
      WA_SAIDA_DC         type TY_SAIDA_D,
      WA_TBSL             type TBSL,
      WA_KNA1             type TY_KNA1,
      WA_BSIS             type TY_BSIS,
      WA_BSAS             type TY_BSAS,
      WA_BSAS_AUX         type TY_BSAS,
      WA_BSET             type TY_BSET,
      WL_BSET             type TY_BSET,
      WA_BSAK             type TY_BSAK,
      WA_BSAD             type TY_BSAD,
      WA_T005T            type TY_T005T,
      WL_T007S            type T007S,
      WA_SKAT             type TY_SKAT,
      WL_T001             type T001,
      WL_T005             type T005,
      WA_ZIB_CONTABIL     type ZIB_CONTABIL,
      WA_ZIB_CONTABIL_AUX type TY_ZIB,
      WA_ZIB_CONTABIL_CHV type ZIB_CONTABIL_CHV,
      WA_ZIB_CONTABIL_TOT type ZIB_CONTABIL.

*----------------------------------------------------------------------*
* Estrutura ALV
*----------------------------------------------------------------------*
data:
  IT_FCAT         type table of TY_ESTRUTURA,
  S_VARIANT       type DISVARIANT,
  T_TOP           type SLIS_T_LISTHEADER,
  XS_EVENTS       type SLIS_ALV_EVENT,
  EVENTS          type SLIS_T_EVENT,
  GD_LAYOUT       type SLIS_LAYOUT_ALV,
  T_PRINT         type SLIS_PRINT_ALV,
  V_REPORT        like SY-REPID,
  T_SORT          type SLIS_T_SORTINFO_ALV with header line,
  IT_SETLEAF      like table of SETLEAF initial size 0 with header line,
  ESTRUTURA       type table of TY_ESTRUTURA,
  VG_I            type I,
  WG_MENSAGEM(30).

ranges: R_IVA for ZIB_CONTABIL-TAX_CODE.

************************************************************************
* Variaveis
************************************************************************

data: XBASE      type ZIB_CONTABIL-WRBTR,
      XBASED     type ZIB_CONTABIL-WRBTR,
      BASE_BSCHL type TBSL-BSCHL,
      FORN_BSCHL type TBSL-BSCHL,
      REVE_BSCHL type TBSL-BSCHL,
      XRATE      type ZKURSF,
      XDMBE2     type ZIB_CONTABIL-DMBE2,
      XIMPOSTOS  type ZIB_CONTABIL-WRBTR,
      XIMPOSTOSD type ZIB_CONTABIL-WRBTR,
      XBUZEIB    type BSIS-BUZEI,
      XBUZEII    type BSIS-BUZEI,
      BASE_SHKZG type TBSL-SHKZG,
      IMPO_SHKZG type TBSL-SHKZG,
      IMPO_BSCHL type TBSL-BSCHL,
      IMPO_HKONT type ZIB_CONTABIL-HKONT,
      VAL_KIDNO  type ZIB_CONTABIL-KIDNO,
      INV_BUDAT  type BKPF-BUDAT,
      INV_BLDAT  type BKPF-BLDAT,
      V_HKONT    type ZIB_CONTABIL-HKONT.

************************************************************************
* Variaveis ALV
************************************************************************
*& Declaração de Objetos/Classes                                      &*
*&--------------------------------------------------------------------&*
data: EDITCONTAINER   type ref to CL_GUI_CUSTOM_CONTAINER,
      CL_CONTAINER    type ref to CL_GUI_CUSTOM_CONTAINER,
      EDITOR          type ref to CL_GUI_TEXTEDIT,
      CL_CONTAINER_95 type ref to CL_GUI_DOCKING_CONTAINER,
      CL_CONTAINER_05 type ref to CL_GUI_DOCKING_CONTAINER,
      OBJ_DYNDOC_ID   type ref to CL_DD_DOCUMENT,
      CL_GRID         type ref to CL_GUI_ALV_GRID,
      WA_STABLE       type LVC_S_STBL,
      WA_AFIELD       type LVC_S_FCAT,
      IT_FIELDCAT     type LVC_T_FCAT,
      W_FIELDCAT      type LVC_S_FCAT,
      I_SORT          type LVC_T_SORT,
      WA_LAYOUT       type LVC_S_LAYO,
      IS_STABLE       type LVC_S_STBL value 'XX',
      WG_REPNAME      like SY-REPID,
      WG_X_VARIANT    like DISVARIANT,
      WG_EXIT(1)      type C,
      WG_SAVE(1)      type C,
      WG_VARIANT      like DISVARIANT,
      GT_F4           type LVC_T_F4 with header line.

constants:
         C_X               type C value 'X'.


************************************************************************
* D E F I N I T I O N
************************************************************************
class LCL_EVENT_RECEIVER definition.
  public section.
    methods:
      CATCH_HOTSPOT
        for event HOTSPOT_CLICK of CL_GUI_ALV_GRID
        importing E_ROW_ID
                  E_COLUMN_ID
                  ES_ROW_NO.


  private section.
endclass.                    "lcl_event_receiver DEFINITION

************************************************************************
* I M P L E M E N T A T I O N
************************************************************************
class LCL_EVENT_RECEIVER implementation.
  method CATCH_HOTSPOT.
    if E_ROW_ID is not initial.
      read table IT_SAIDA_D into WA_SAIDA_D index E_ROW_ID.
      if E_COLUMN_ID = 'BELNR' and WA_SAIDA_D-BELNR is not initial.
        set parameter id 'BLN' field WA_SAIDA_D-BELNR.
        set parameter id 'BUK' field WA_SAIDA_D-BUKRS.
        set parameter id 'GJR' field WA_SAIDA_D-BUDAT+6(4).
        call transaction 'FB03' and skip first screen.
      endif.
    endif.
  endmethod.                    "CATCH_HOTSPOT



endclass.                    "LCL_EVENT_RECEIVER IMPLEMENTATION



data:       EVENT_RECEIVER   type ref to LCL_EVENT_RECEIVER.
*----------------------------------------------------------------------*
* TELA DE SELEÇÃO
*----------------------------------------------------------------------*

selection-screen: begin of block B1 with frame title text-001.
  select-options:  P_BUKRS  for ZIB_CONTABIL-BUKRS no intervals no-extension ,
                   P_BLDAT  for BKPF-BLDAT obligatory,
                   P_BUDAT  for BKPF-BUDAT ,
                   P_LAND1  for ZIB_CONTABIL-LAND1 no intervals no-extension.
selection-screen: end of block B1.

selection-screen: begin of block B2 with frame title text-002.
  parameters:
    R_TOT radiobutton group RAD1 default 'X',
    R_DET radiobutton group RAD1.
selection-screen: end of block B2.

initialization.
  select single *
     from USR05
     into @data(_USR05)
     where BNAME = @SY-UNAME
     and PARID   = 'BUK'.
  if SY-SUBRC = 0.
    P_BUKRS-SIGN    = 'I'.
    P_BUKRS-OPTION  = 'EQ'.
    P_BUKRS-LOW = _USR05-PARVA+0(4).
    append P_BUKRS.
  endif.
*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
start-of-selection.

  perform:
            F_SELECIONA_DADOS, " Form seleciona dados
            F_SELECIONA_DADOS_SAP, " Form seleciona dados
            F_SAIDA, " Form de saida
            F_SAIDA_SAP, " Form de saida
            F_IMPRIME_DADOS.

end-of-selection.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_SELECIONA_DADOS .
  data TABIX type SY-TABIX.
*

  call function 'G_SET_GET_ALL_VALUES'
    exporting
      CLASS         = '0000'
      SETNR         = 'MAGGI_0201_VAT'
    tables
      SET_VALUES    = T_VAT201
    exceptions
      SET_NOT_FOUND = 1
      others        = 2.
  if SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.
  sort T_VAT201 by FROM.

  select ZIB_CONTABIL~OBJ_KEY
         ZIB_CONTABIL~TAX_CODE
         ZIB_CONTABIL~LAND1
         ZIB_CONTABIL~HKONT
         ZIB_CONTABIL~BSCHL
         ZIB_CONTABIL~BUDAT
         ZIB_CONTABIL~BLDAT
         ZIB_CONTABIL~ZUONR
         ZIB_CONTABIL~XBLNR
         ZIB_CONTABIL~BUKRS
         ZIB_CONTABIL~WAERS
         ZIB_CONTABIL~CONTROLE_VAT
         ZIB_CONTABIL~SEQITEM
         ZIB_CONTABIL~WRBTR
         ZIB_CONTABIL~DMBE2
         ZIB_CONTABIL~KIDNO
         ZIB_CONTABIL~SGTXT
         BKPF~STBLG
         BKPF~STJAH
    from BKPF
    inner join ZIB_CONTABIL on BKPF~AWKEY  = ZIB_CONTABIL~OBJ_KEY
    into table IT_ZIB_CONTABIL_AUX
    where BKPF~BUKRS in P_BUKRS
    and   BKPF~BUDAT in P_BUDAT
    and   BKPF~BLDAT in P_BLDAT
    and   ZIB_CONTABIL~TAX_CODE ne ''
    and   ZIB_CONTABIL~CONTROLE_VAT ne ''
    and   ZIB_CONTABIL~LAND1    in P_LAND1.


  check IT_ZIB_CONTABIL_AUX[] is not initial.

  select LAND1 LANDX
    from T005T
    into table IT_T005T
    for all entries in IT_ZIB_CONTABIL_AUX
    where LAND1 eq IT_ZIB_CONTABIL_AUX-LAND1
    and SPRAS = 'EN'.

  select  *
    from T005
    into table IT_T005
      for all entries in IT_ZIB_CONTABIL_AUX
      where LAND1 eq IT_ZIB_CONTABIL_AUX-LAND1.

  select *
    from T007S
    into corresponding fields of table TL_T007S
    for all entries in IT_T005
       where SPRAS eq SY-LANGU
         and KALSM eq IT_T005-KALSM.

  sort: IT_T005  by LAND1,
        TL_T007S by KALSM MWSKZ.

  R_IVA-SIGN = 'I'.
  R_IVA-OPTION = 'EQ'.
  R_IVA-LOW = 'A0.'.
  append R_IVA.
  R_IVA-SIGN = 'I'.
  R_IVA-OPTION = 'EQ'.
  R_IVA-LOW = 'G1'.
  append R_IVA.
  R_IVA-SIGN = 'I'.
  R_IVA-OPTION = 'EQ'.
  R_IVA-LOW = 'G2'.
  append R_IVA.
  R_IVA-SIGN = 'I'.
  R_IVA-OPTION = 'EQ'.
  R_IVA-LOW = 'G3'.
  append R_IVA.
  R_IVA-SIGN = 'I'.
  R_IVA-OPTION = 'EQ'.
  R_IVA-LOW = 'G4'.
  append R_IVA.
  R_IVA-SIGN = 'I'.
  R_IVA-OPTION = 'EQ'.
  R_IVA-LOW = 'G5'.
  append R_IVA.
  R_IVA-SIGN = 'I'.
  R_IVA-OPTION = 'EQ'.
  R_IVA-LOW = 'G6'.
  append R_IVA.
  R_IVA-SIGN = 'I'.
  R_IVA-OPTION = 'EQ'.
  R_IVA-LOW = 'G7'.
  append R_IVA.
  R_IVA-SIGN = 'I'.
  R_IVA-OPTION = 'EQ'.
  R_IVA-LOW = 'G8'.
  append R_IVA.
  R_IVA-SIGN = 'I'.
  R_IVA-OPTION = 'EQ'.
  R_IVA-LOW = 'G9'.
  append R_IVA.

  "exceção para A0
  IT_ZIB_CONTABIL_A0[] = IT_ZIB_CONTABIL_AUX[].
  delete IT_ZIB_CONTABIL_A0  where TAX_CODE not in R_IVA.

  delete IT_ZIB_CONTABIL_AUX where TAX_CODE in  R_IVA.

  loop at IT_ZIB_CONTABIL_AUX into WA_ZIB_CONTABIL_AUX.
    if  WA_ZIB_CONTABIL_AUX-TAX_CODE in R_IVA.
      continue.
    endif.
    TABIX = SY-TABIX.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = WA_ZIB_CONTABIL_AUX-HKONT
      importing
        OUTPUT = V_HKONT.
    read table T_VAT201 with key FROM = V_HKONT. "vat
    if SY-SUBRC ne 0.
      WA_ZIB_CONTABIL_AUX-DEL = 'X'.
      modify IT_ZIB_CONTABIL_AUX from WA_ZIB_CONTABIL_AUX index TABIX transporting DEL.
    endif.
  endloop.

  delete IT_ZIB_CONTABIL_AUX where DEL = 'X'.

  delete IT_ZIB_CONTABIL_AUX where TAX_CODE = ''.

  " Separa por Tax_code o lancamento
  sort IT_ZIB_CONTABIL_AUX by OBJ_KEY CONTROLE_VAT.
  delete adjacent duplicates from IT_ZIB_CONTABIL_AUX comparing OBJ_KEY CONTROLE_VAT.

  "insere A) sem agrupar
  loop at IT_ZIB_CONTABIL_A0 into WA_ZIB_CONTABIL_AUX.
    append WA_ZIB_CONTABIL_AUX to IT_ZIB_CONTABIL_AUX.
  endloop.
  sort IT_ZIB_CONTABIL_AUX by OBJ_KEY CONTROLE_VAT.

  check IT_ZIB_CONTABIL_AUX[] is not initial.

  select *
    from ZIB_CONTABIL_CHV
    into table IT_ZIB_CONTABIL_CHV
    for all entries in IT_ZIB_CONTABIL_AUX
    where OBJ_KEY = IT_ZIB_CONTABIL_AUX-OBJ_KEY.


  select  BUKRS BELNR GJAHR BUZEI DMBTR
    from BSIS
    into table IT_BSIS
    for all entries in IT_ZIB_CONTABIL_CHV
    where BUKRS = IT_ZIB_CONTABIL_CHV-BUKRS
    and   BELNR = IT_ZIB_CONTABIL_CHV-BELNR
    and   GJAHR = IT_ZIB_CONTABIL_CHV-GJAHR.

  select *
  from ZIB_CONTABIL
  into table IT_ZIB_CONTABIL
  for all entries in IT_ZIB_CONTABIL_AUX
  where OBJ_KEY eq IT_ZIB_CONTABIL_AUX-OBJ_KEY.

  IT_ZIB_CONTABIL_F[] = IT_ZIB_CONTABIL[].
  delete IT_ZIB_CONTABIL_F where BSCHL eq '40' or BSCHL eq '50'.

  delete IT_ZIB_CONTABIL   where BSCHL ne '40' and BSCHL ne '50'.

  loop at IT_ZIB_CONTABIL_F into WA_ZIB_CONTABIL.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = WA_ZIB_CONTABIL-HKONT
      importing
        OUTPUT = WA_ZIB_CONTABIL-HKONT.
    "
    modify IT_ZIB_CONTABIL_F from WA_ZIB_CONTABIL index SY-TABIX transporting HKONT.
  endloop.

  select KUNNR NAME1 LAND1
    from KNA1
    into table IT_KNA1
    for all entries in IT_ZIB_CONTABIL_F
    where KUNNR eq IT_ZIB_CONTABIL_F-HKONT.


  select LIFNR NAME1
    from LFA1
    appending table IT_KNA1
    for all entries in IT_ZIB_CONTABIL_F
    where LIFNR eq IT_ZIB_CONTABIL_F-HKONT.


endform.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
form F_SELECIONA_DADOS_SAP.
  data: TABIX type SY-TABIX.

  select *
  from  SETLEAF
  into table T_SET
  where SETCLASS      = '0000'
  and   SETNAME        = 'MAGGI_0201_VAT'.

  select *
    from SETLINET
    into table T_LAY
    for all entries in T_SET
    where SETCLASS   = T_SET-SETCLASS
    and SUBCLASS     = T_SET-SUBCLASS
    and SETNAME      = T_SET-SETNAME
    and LANGU        = 'P'
    and LINEID       = T_SET-LINEID.

  if SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  endif.

  select BSAS~BUKRS BSAS~BUDAT BSAS~BLDAT BSAS~HKONT BSAS~GJAHR BSAS~BELNR BSAS~WAERS BKPF~STBLG  BKPF~STJAH BSAS~DMBTR BSAS~DMBE2 BSAS~SHKZG BSAS~BLART BSAS~BSCHL BSAS~SGTXT
    from BSAS
    inner join BKPF on BKPF~BUKRS = BSAS~BUKRS
                   and BKPF~BELNR = BSAS~BELNR
                   and BKPF~GJAHR = BSAS~GJAHR
                   and BKPF~TCODE ne 'FB08'
    into table IT_BSAS
  where BSAS~BUKRS in P_BUKRS
  and   BSAS~BUDAT in P_BUDAT
  and   BSAS~BLDAT in P_BLDAT.

  select BSIS~BUKRS BSIS~BUDAT BSIS~BLDAT BSIS~HKONT BSIS~GJAHR BSIS~BELNR BSIS~WAERS BKPF~STBLG  BKPF~STJAH BSIS~DMBTR BSIS~DMBE2 BSIS~SHKZG BSIS~BLART  BSIS~BSCHL BSIS~SGTXT
    from BSIS
    inner join BKPF on BKPF~BUKRS = BSIS~BUKRS
                   and BKPF~BELNR = BSIS~BELNR
                   and BKPF~GJAHR = BSIS~GJAHR
                   and BKPF~TCODE ne 'FB08'
    appending table IT_BSAS
  where BSIS~BUKRS in P_BUKRS
  and   BSIS~BUDAT in P_BUDAT
  and   BSIS~BLDAT in P_BLDAT.

  check IT_BSAS[] is not initial.

  IT_BSAS_AUX[] = IT_BSAS[].

  select BUKRS BELNR GJAHR HWBAS FWBAS HWSTE HKONT SHKZG LSTML MWSKZ
  from BSET
  into table IT_BSET
  for all entries in IT_BSAS
  where BUKRS  = IT_BSAS-BUKRS
  and   BELNR  = IT_BSAS-BELNR
  and   GJAHR  = IT_BSAS-GJAHR.
*  AND   LSTML  IN P_LAND1.

  sort  IT_BSET by BUKRS BELNR GJAHR.


  loop at IT_BSAS into WA_BSAS.
    TABIX = SY-TABIX.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = WA_BSAS-HKONT
      importing
        OUTPUT = V_HKONT.
    read table T_VAT201 with key FROM = V_HKONT. "vat
    if SY-SUBRC ne 0.
      read table IT_BSET into WA_BSET with key BUKRS  = WA_BSAS-BUKRS
                                               BELNR  = WA_BSAS-BELNR
                                               GJAHR  = WA_BSAS-GJAHR binary search.
      if SY-SUBRC ne 0.
        WA_BSAS-DEL = 'X'.
        modify IT_BSAS from WA_BSAS index TABIX transporting DEL.
      endif.
    endif.
  endloop.
  delete IT_BSAS where DEL = 'X'.

  check IT_BSAS[] is not initial.

*  SELECT *
*    FROM    TBSL
*    INTO TABLE IT_TBSL_SAP
*    FOR ALL ENTRIES IN IT_BSAS
*    WHERE BSCHL = IT_BSAS-BSCHL.

  "fornecedor
  select BUKRS BELNR GJAHR LIFNR XBLNR
    from BSIK
    into table IT_BSAK
    for all entries in IT_BSAS
   where BUKRS  = IT_BSAS-BUKRS
   and   BELNR  = IT_BSAS-BELNR
   and   GJAHR  = IT_BSAS-GJAHR.

  select BUKRS BELNR GJAHR LIFNR XBLNR
    from BSAK
    appending table IT_BSAK
    for all entries in IT_BSAS
   where BUKRS  = IT_BSAS-BUKRS
   and   BELNR  = IT_BSAS-BELNR
   and   GJAHR  = IT_BSAS-GJAHR.

  "cliente
  select BUKRS BELNR GJAHR KUNNR XBLNR
      from BSID
      into table IT_BSAD
      for all entries in IT_BSAS
     where BUKRS  = IT_BSAS-BUKRS
     and   BELNR  = IT_BSAS-BELNR
     and   GJAHR  = IT_BSAS-GJAHR.

  select BUKRS BELNR GJAHR KUNNR XBLNR
    from BSAD
    appending table IT_BSAD
    for all entries in IT_BSAS
   where BUKRS  = IT_BSAS-BUKRS
   and   BELNR  = IT_BSAS-BELNR
   and   GJAHR  = IT_BSAS-GJAHR.

  if IT_BSAK[] is not initial.
    select LIFNR NAME1
      from LFA1
      appending table IT_KNA1
      for all entries in IT_BSAK
      where LIFNR eq  IT_BSAK-LIFNR.
  endif.

  if IT_BSAD[] is not initial.
    select KUNNR NAME1 LAND1
      from KNA1
      appending table IT_KNA1
      for all entries in IT_BSAD
      where KUNNR eq  IT_BSAD-KUNNR.
  endif.

  if IT_BSET[] is not initial.
    select LAND1 LANDX
      from T005T
      appending table IT_T005T
      for all entries in IT_BSET
      where LAND1 eq IT_BSET-LSTML
      and SPRAS = 'EN'.
  endif.

endform.                    "F_SELECIONA_DADOS_sap
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_SAIDA .
  data: VG_TYPE(20),
        VG_TYPE_C(20),
        WL_TBSL type TBSL.

  IT_LFA1[] = IT_KNA1[].

  delete  IT_LFA1 where LAND1 ne ''.
  delete  IT_KNA1 where LAND1 eq ''.

  sort: IT_TBSL             by BSCHL,
        IT_ZIB_CONTABIL     by OBJ_KEY CONTROLE_VAT  TAX_CODE,
        IT_ZIB_CONTABIL_AUX by OBJ_KEY CONTROLE_VAT  TAX_CODE,
        IT_ZIB_CONTABIL_F   by OBJ_KEY,
        IT_ZIB_CONTABIL_CHV by OBJ_KEY,
        IT_T005T            by LAND1,
        IT_KNA1             by KUNNR,
        IT_LFA1             by KUNNR,
        IT_BSIS             by BUKRS BELNR GJAHR BUZEI.


  loop at IT_ZIB_CONTABIL_AUX into WA_ZIB_CONTABIL_AUX.
    read table IT_ZIB_CONTABIL into WA_ZIB_CONTABIL with key OBJ_KEY        = WA_ZIB_CONTABIL_AUX-OBJ_KEY
                                                             CONTROLE_VAT   = WA_ZIB_CONTABIL_AUX-CONTROLE_VAT
                                                             TAX_CODE       = WA_ZIB_CONTABIL_AUX-TAX_CODE binary search.
    clear : XDMBE2 ,XRATE,XBASE,BASE_SHKZG,BASE_BSCHL,FORN_BSCHL,IMPO_HKONT,XBUZEIB,XBASED,XIMPOSTOSD,VAL_KIDNO.
    if WA_ZIB_CONTABIL_AUX-TAX_CODE in R_IVA.
      if WA_ZIB_CONTABIL_AUX-KIDNO is not initial.
        VAL_KIDNO = WA_ZIB_CONTABIL_AUX-KIDNO.
      endif.
      BASE_BSCHL = WA_ZIB_CONTABIL_AUX-BSCHL.
      IMPO_BSCHL = WA_ZIB_CONTABIL_AUX-BSCHL.
      XBUZEIB    = WA_ZIB_CONTABIL_AUX-SEQITEM.
      XBASE      = WA_ZIB_CONTABIL_AUX-WRBTR.
      XDMBE2     = WA_ZIB_CONTABIL_AUX-DMBE2.
      if WA_ZIB_CONTABIL_AUX-DMBE2 gt 0.
        XRATE      = WA_ZIB_CONTABIL_AUX-DMBE2 / WA_ZIB_CONTABIL_AUX-WRBTR.
      endif.
      "Verifica cliente ou fornecedor
      read table IT_ZIB_CONTABIL_F into WA_ZIB_CONTABIL with key OBJ_KEY  = WA_ZIB_CONTABIL_AUX-OBJ_KEY binary search.
      FORN_BSCHL = WA_ZIB_CONTABIL-BSCHL.
    else.
      XIMPOSTOS  = WA_ZIB_CONTABIL-WRBTR.
      IMPO_HKONT = WA_ZIB_CONTABIL-HKONT.
      XBUZEII    = WA_ZIB_CONTABIL-SEQITEM.
      IMPO_BSCHL = WA_ZIB_CONTABIL-BSCHL.
      "Verifica cliente ou fornecedor
      read table IT_ZIB_CONTABIL_F into WA_ZIB_CONTABIL with key OBJ_KEY  = WA_ZIB_CONTABIL_AUX-OBJ_KEY binary search.
      FORN_BSCHL = WA_ZIB_CONTABIL-BSCHL.
      "
      loop at IT_ZIB_CONTABIL into WA_ZIB_CONTABIL where OBJ_KEY      = WA_ZIB_CONTABIL_AUX-OBJ_KEY
                                                   and   CONTROLE_VAT = WA_ZIB_CONTABIL_AUX-CONTROLE_VAT
                                                   and   TAX_CODE     = WA_ZIB_CONTABIL_AUX-TAX_CODE.
        if WA_ZIB_CONTABIL-TAX_CODE in R_IVA.
          continue.
        endif.
        if WA_ZIB_CONTABIL-KIDNO is not initial.
          VAL_KIDNO = WA_ZIB_CONTABIL-KIDNO.
        endif.

        if XBASE lt WA_ZIB_CONTABIL-WRBTR.
          BASE_BSCHL = WA_ZIB_CONTABIL-BSCHL.
          XBUZEIB    = WA_ZIB_CONTABIL-SEQITEM.
          XBASE      = WA_ZIB_CONTABIL-WRBTR.
          XDMBE2     = WA_ZIB_CONTABIL-DMBE2.
          if WA_ZIB_CONTABIL-DMBE2 gt 0.
            XRATE      = WA_ZIB_CONTABIL-DMBE2 / WA_ZIB_CONTABIL-WRBTR.
          endif.
        endif.

      endloop.
    endif.

    loop at IT_ZIB_CONTABIL into WA_ZIB_CONTABIL where OBJ_KEY      = WA_ZIB_CONTABIL_AUX-OBJ_KEY
                                                   and CONTROLE_VAT = WA_ZIB_CONTABIL_AUX-CONTROLE_VAT
                                                   and TAX_CODE     = WA_ZIB_CONTABIL_AUX-TAX_CODE.

      if WA_ZIB_CONTABIL-TAX_CODE not in R_IVA.
        if WA_ZIB_CONTABIL-WRBTR ge ABS( XBASE ).
          continue.
        endif.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            INPUT  = WA_ZIB_CONTABIL-HKONT
          importing
            OUTPUT = V_HKONT.

        read table T_VAT201 with key FROM = V_HKONT. "vat
        if SY-SUBRC = 0.
          XBUZEII    = WA_ZIB_CONTABIL-SEQITEM.
          IMPO_BSCHL = WA_ZIB_CONTABIL-BSCHL.
          IMPO_HKONT = WA_ZIB_CONTABIL-HKONT.
          XIMPOSTOS = WA_ZIB_CONTABIL-WRBTR.
        else.
          continue.
        endif.
      endif.

      if IMPO_BSCHL = '50'. "Mesmo sinal imposto
        XBASE  = XBASE * -1.
      endif.
      "
      "Zera se não achou no SET
      if IMPO_HKONT is initial.
        XIMPOSTOS = 0.
      endif.

      if IMPO_BSCHL = '50'.
        XIMPOSTOS = XIMPOSTOS * -1.
      endif.

      if  WA_ZIB_CONTABIL_AUX-TAX_CODE in R_IVA.
        XIMPOSTOS = 0.
        clear: XBUZEII, IMPO_HKONT.
      endif.

      if WA_ZIB_CONTABIL_AUX-WAERS = 'USD'.
        XBASED       = XBASE.
        XIMPOSTOSD   = XIMPOSTOS.
        read table IT_ZIB_CONTABIL_CHV into WA_ZIB_CONTABIL_CHV with key OBJ_KEY = WA_ZIB_CONTABIL_AUX-OBJ_KEY binary search.
        if SY-SUBRC = 0.
          read table IT_BSIS into WA_BSIS with key BUKRS = WA_ZIB_CONTABIL_CHV-BUKRS
                                                   BELNR = WA_ZIB_CONTABIL_CHV-BELNR
                                                   GJAHR = WA_ZIB_CONTABIL_CHV-GJAHR
                                                   BUZEI = XBUZEIB binary search.
          if SY-SUBRC = 0.
            XBASE = WA_BSIS-DMBTR.
            if IMPO_BSCHL = '50'. "Mesmo sinal imposto
              XBASE  = XBASE * -1.
            endif.
            " Se tiver USD o rate será EUR / USD
            XRATE =  XBASED /  XBASE.
          else.
            clear XBASE.
          endif.
          read table IT_BSIS into WA_BSIS with key BUKRS = WA_ZIB_CONTABIL_CHV-BUKRS
                                                   BELNR = WA_ZIB_CONTABIL_CHV-BELNR
                                                   GJAHR = WA_ZIB_CONTABIL_CHV-GJAHR
                                                   BUZEI = XBUZEII binary search.
          if SY-SUBRC = 0.
            XIMPOSTOS = WA_BSIS-DMBTR.
            if IMPO_BSCHL = '50'.
              XIMPOSTOS = XIMPOSTOS * -1.
            endif.
          else.
            clear XIMPOSTOS.
          endif.
          if  WA_ZIB_CONTABIL_AUX-TAX_CODE in R_IVA.
            XIMPOSTOS = 0.
          endif.
        else.
          clear: XBASE, XIMPOSTOS.
        endif.
      endif.

      call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
        exporting
          INPUT  = IMPO_HKONT
        importing
          OUTPUT = IMPO_HKONT.

      if R_TOT = 'X'.
        clear VG_TYPE.
        "Exceção

        if '31_32' cs FORN_BSCHL.
          VG_TYPE      = 'To Claim'.
        elseif '21_22' cs FORN_BSCHL.
          VG_TYPE      = 'Negative to Claim'.
        elseif '01_02' cs FORN_BSCHL.
          VG_TYPE       = 'To Pay'.
        elseif '11_12' cs FORN_BSCHL.
          VG_TYPE       = 'Negative to Pay'.
        endif.

        if WA_ZIB_CONTABIL_AUX-TAX_CODE in R_IVA.
          concatenate WA_ZIB_CONTABIL_AUX-TAX_CODE '-' VAL_KIDNO into VAL_KIDNO.
        else.
          VAL_KIDNO = WA_ZIB_CONTABIL_AUX-TAX_CODE.
        endif.

        read table IT_SAIDA into WA_SAIDA with key LAND1    = WA_ZIB_CONTABIL_AUX-LAND1
                                                   TAX_CODE = VAL_KIDNO
                                                   TYPE     = VG_TYPE.
        if WA_ZIB_CONTABIL_AUX-STBLG is not initial.
          continue.
        endif.
        if SY-SUBRC ne 0.
          read table IT_T005T into WA_T005T with key LAND1 = WA_ZIB_CONTABIL_AUX-LAND1 binary search.
          WA_SAIDA-LAND1      = WA_ZIB_CONTABIL_AUX-LAND1.
          concatenate WA_ZIB_CONTABIL_AUX-LAND1 '-' WA_T005T-LANDX into  WA_SAIDA-LANDX.
          WA_SAIDA-TAX_CODE   = VAL_KIDNO.
          WA_SAIDA-TYPE       = VG_TYPE.
          WA_SAIDA-KOART      = WA_TBSL-KOART.
          WA_SAIDA-VAT_BASIS  = XBASE.
          WA_SAIDA-RATE       = XRATE.
          WA_SAIDA-DMBE2      = XDMBE2.
          WA_SAIDA-VAT        = XIMPOSTOS.

          WA_SAIDA-VAT_BASISD  = XBASED.
          WA_SAIDA-VATD        = XIMPOSTOSD.

          if XBASE ne 0.
            WA_SAIDA-VATP       = (  XIMPOSTOS / XBASE ) * 100.
          endif.

          WA_SAIDA-QTDE       = 1.
          append WA_SAIDA to IT_SAIDA.
          clear WA_SAIDA.
        else.
          add  XBASE     to WA_SAIDA-VAT_BASIS.
          add  XIMPOSTOS to WA_SAIDA-VAT.

          add  XBASED     to WA_SAIDA-VAT_BASISD.
          add  XIMPOSTOSD to WA_SAIDA-VATD.

          add  XDMBE2    to WA_SAIDA-DMBE2.
          add  1         to WA_SAIDA-QTDE.

          if WA_SAIDA-VAT_BASIS gt 0.
            WA_SAIDA-RATE = WA_SAIDA-DMBE2 / WA_SAIDA-VAT_BASIS.
            WA_SAIDA-VATP = (  WA_SAIDA-VAT / WA_SAIDA-VAT_BASIS ) * 100.
          endif.
          modify IT_SAIDA from WA_SAIDA index SY-TABIX transporting VAT_BASIS VAT VATP QTDE VAT_BASISD VATD .
        endif.

      else.
        clear: VG_TYPE_C , VG_TYPE.

        if '31_32' cs FORN_BSCHL.
          VG_TYPE      = 'To Claim'.
          VG_TYPE_C    = 'To Claim'.
        elseif '21_22' cs FORN_BSCHL.
          VG_TYPE      = 'Negative to Claim'.
          VG_TYPE_C    = 'To Claim'.
        elseif '01_02' cs FORN_BSCHL.
          VG_TYPE       = 'To Pay'.
          VG_TYPE_C     = 'To Pay'.
        elseif '11_12' cs FORN_BSCHL.
          VG_TYPE       = 'Negative to Pay'.
          VG_TYPE_C     = 'To Pay'.
        endif.

        clear WA_SAIDA_D-SGTXT.
        read table IT_ZIB_CONTABIL_F into WA_ZIB_CONTABIL with key OBJ_KEY  = WA_ZIB_CONTABIL_AUX-OBJ_KEY binary search.
        if SY-SUBRC = 0.
          WA_SAIDA_D-SGTXT = WA_ZIB_CONTABIL-SGTXT.
          call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
            exporting
              INPUT  = WA_ZIB_CONTABIL-HKONT
            importing
              OUTPUT = WA_SAIDA_D-HKONT.
          "
          clear WA_KNA1.
          if VG_TYPE_C = 'To Pay'.
            read table IT_KNA1 into WA_KNA1 with key KUNNR = WA_ZIB_CONTABIL-HKONT  binary search.
          else.
            read table IT_LFA1 into WA_KNA1 with key KUNNR = WA_ZIB_CONTABIL-HKONT  binary search.
          endif.
          if SY-SUBRC = 0.
            WA_SAIDA_D-NAME1      = WA_KNA1-NAME1.
          endif.
        endif.

        read table IT_ZIB_CONTABIL_CHV into WA_ZIB_CONTABIL_CHV with key OBJ_KEY = WA_ZIB_CONTABIL_AUX-OBJ_KEY binary search.
        if SY-SUBRC = 0.
          WA_SAIDA_D-BELNR = WA_ZIB_CONTABIL_CHV-BELNR.
          WA_SAIDA_D-STBLG = WA_ZIB_CONTABIL_AUX-STBLG.
        endif.

        WA_SAIDA_D-BUKRS      = WA_ZIB_CONTABIL_AUX-BUKRS.
        WA_SAIDA_D-BUDAT      = WA_ZIB_CONTABIL_AUX-BUDAT.
        WA_SAIDA_D-BLDAT      = WA_ZIB_CONTABIL_AUX-BLDAT.
        WA_SAIDA_D-ZUONR      = WA_ZIB_CONTABIL_AUX-ZUONR.
        WA_SAIDA_D-XBLNR      = WA_ZIB_CONTABIL_AUX-XBLNR.
        WA_SAIDA_D-TAX_CODE   = WA_ZIB_CONTABIL_AUX-TAX_CODE.

        read table IT_T005T into WA_T005T with key LAND1 = WA_ZIB_CONTABIL_AUX-LAND1 binary search.
        concatenate WA_ZIB_CONTABIL_AUX-LAND1 '-' WA_T005T-LANDX into  WA_SAIDA_D-LANDX.

        WA_SAIDA_D-TYPE        = VG_TYPE.

        WA_SAIDA_D-VAT_BASIS  = XBASE.
        WA_SAIDA_D-RATE       = XRATE.
        WA_SAIDA_D-VAT        = XIMPOSTOS.


        WA_SAIDA_D-VAT_BASISD  = XBASED.
        WA_SAIDA_D-VATD        = XIMPOSTOSD.

        WA_SAIDA_D-VATP = 0.
        if XBASE ne 0.
          WA_SAIDA_D-VATP       = (  XIMPOSTOS / XBASE ) * 100.
        endif.

        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            INPUT  = IMPO_HKONT
          importing
            OUTPUT = WA_SAIDA_D-HKONTV.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            INPUT  = IMPO_HKONT
          importing
            OUTPUT = IMPO_HKONT.


        read table IT_T005 into WL_T005 with key LAND1 = WA_ZIB_CONTABIL_AUX-LAND1 binary search.

        read table TL_T007S into WL_T007S with key KALSM = WL_T005-KALSM
                                                   MWSKZ = WA_SAIDA_D-TAX_CODE binary search.
        if SY-SUBRC = 0.
          WA_SAIDA_D-TXT50 = WL_T007S-TEXT1.
        elseif IMPO_HKONT is not initial.
          select single TXT20 into WA_SAIDA_D-TXT50
             from SKAT
             where SAKNR eq IMPO_HKONT
             and   SPRAS eq 'EN'
             and   KTOPL eq '0050'.
        else.
          if  WA_ZIB_CONTABIL_AUX-TAX_CODE in R_IVA.
            concatenate 'VAT' '0%' into WA_SAIDA_D-TXT50 separated by SPACE.
            if VAL_KIDNO is not initial.
              WA_SAIDA_D-TXT50 = VAL_KIDNO.
            endif.
          endif.
        endif.

        if WA_ZIB_CONTABIL_AUX+0(5) = 'ZGL17'.
          WA_SAIDA_D-ORIGEM = 'SAP_ZGL'.
        else.
          WA_SAIDA_D-ORIGEM = 'Comex'.
        endif.


        append WA_SAIDA_D to IT_SAIDA_D.
        if WA_SAIDA_D-STBLG is not initial.
          clear VG_TYPE.
          move-corresponding WA_SAIDA_D to WA_SAIDA_DC.
          WA_SAIDA_DC-BELNR       = WA_SAIDA_D-STBLG.
          WA_SAIDA_DC-STBLG       = WA_SAIDA_D-BELNR.
          WA_SAIDA_DC-VAT_BASIS   = WA_SAIDA_D-VAT_BASIS * -1.
          WA_SAIDA_DC-VAT         = WA_SAIDA_D-VAT * -1.
          WA_SAIDA_DC-VAT_BASISD  = WA_SAIDA_D-VAT_BASISD * -1.
          WA_SAIDA_DC-VATD        = WA_SAIDA_D-VATD * -1.
          WA_SAIDA_DC-TYPE        = VG_TYPE.
          select single BUDAT BLDAT
            from BKPF
            into ( INV_BUDAT, INV_BLDAT )
          where BUKRS = WA_ZIB_CONTABIL_AUX-BUKRS
          and   BELNR = WA_ZIB_CONTABIL_AUX-STBLG
          and   GJAHR = WA_ZIB_CONTABIL_AUX-STJAH.

          concatenate INV_BUDAT+6(2) '.' INV_BUDAT+4(2) '.' INV_BUDAT+0(4) into WA_SAIDA_DC-BUDAT.
          concatenate INV_BLDAT+6(2) '.' INV_BLDAT+4(2) '.' INV_BLDAT+0(4) into WA_SAIDA_DC-BLDAT.

          clear REVE_BSCHL.
* ---> S4 Migration - 15/06/2023 - MA
*          SELECT SINGLE BSCHL
*             INTO REVE_BSCHL
*            FROM BSEG
*          WHERE BUKRS = WA_ZIB_CONTABIL_AUX-BUKRS
*          AND   BELNR = WA_ZIB_CONTABIL_AUX-STBLG
*          AND   GJAHR = WA_ZIB_CONTABIL_AUX-STJAH
*          AND   BSCHL IN ( '01', '02', '11', '12', '21', '22', '31', '32' ).

          data:   LT_BSEG type FAGL_T_BSEG.

          types LR_BSCHL_TYPE type range of BSCHL.

          data : LR_BSCHL type LR_BSCHL_TYPE.
          data: IT_BSEG  type FAGL_T_BSEG,
                LV_BELNR type BELNR_D,
                LV_BUKRS type BUKRS,
                LV_GJAHR type GJAHR.

          LV_BELNR = WA_ZIB_CONTABIL_AUX-STBLG.
          LV_GJAHR  = WA_ZIB_CONTABIL_AUX-STJAH.


          LR_BSCHL = value LR_BSCHL_TYPE( let S = 'I'
                                              O = 'BT'
                                          in SIGN   = S
                                             OPTION = O
                                             ( LOW = '01' )
                                             ( LOW = '02' )
                                             ( LOW = '11' )
                                             ( LOW = '12' )
                                             ( LOW = '21' )
                                             ( LOW = '22' )
                                             ( LOW = '31' )
                                             ( LOW = '32' ) ).

          call function 'FAGL_GET_BSEG'
            exporting
              I_BUKRS = WA_ZIB_CONTABIL_AUX-BUKRS
              I_BELNR = LV_BELNR
              I_GJAHR = LV_GJAHR
            importing
              ET_BSEG = LT_BSEG
            exceptions
              others  = 2.

          delete LT_BSEG where BSCHL not in LR_BSCHL.

          read table LT_BSEG into data(WA_BSEG) index 1.

          if SY-SUBRC eq 0.
            REVE_BSCHL = WA_BSEG-BSCHL.
          endif.
*<--- S4 Migration - 15/06/2023 - MA

          if '31_32' cs REVE_BSCHL.
            VG_TYPE      = 'To Claim'.
            VG_TYPE_C    = 'To Claim'.
          elseif '21_22' cs REVE_BSCHL.
            VG_TYPE      = 'Negative to Claim'.
            VG_TYPE_C    = 'To Claim'.
          elseif '01_02' cs REVE_BSCHL.
            VG_TYPE       = 'To Pay'.
            VG_TYPE_C     = 'To Pay'.
          elseif '11_12' cs REVE_BSCHL.
            VG_TYPE      = 'Negative to Pay'.
            VG_TYPE_C    = 'To Pay'.
          endif.

          WA_SAIDA_DC-TYPE        = VG_TYPE.

          append WA_SAIDA_DC to IT_SAIDA_D.
        endif.
        clear WA_SAIDA_D.
      endif.
    endloop.

  endloop.

endform.                    " F_SAIDA

*&---------------------------------------------------------------------*
*&      Form  F_SAIDA_SAP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form F_SAIDA_SAP .
  data: VG_TYPE(20),
        WL_T001 type T001.

  sort: IT_BSAK by BUKRS BELNR GJAHR,
        IT_BSAD by BUKRS BELNR GJAHR,
        IT_BSET by BUKRS BELNR GJAHR,
        IT_BSAS_AUX by BUKRS BELNR GJAHR,
        IT_TBSL_SAP by BSCHL.


  sort IT_BSAS  by  BUKRS BELNR GJAHR.
  delete adjacent duplicates from IT_BSAS comparing BUKRS BELNR GJAHR.

  refresh IT_BSET_AUX.
  loop at IT_BSAS into WA_BSAS.
    if  WA_BSAS-BLART = 'SI'. "comex
      continue.
    endif.
    clear WA_BSET.
    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        INPUT  = WA_BSAS-HKONT
      importing
        OUTPUT = V_HKONT.
    read table T_SET with key VALFROM =  V_HKONT.
    if SY-SUBRC = 0.
      read table T_LAY with key  SETCLASS   = T_SET-SETCLASS
                              SUBCLASS   = T_SET-SUBCLASS
                              SETNAME    = T_SET-SETNAME
                              LINEID     = T_SET-LINEID.
      if SY-SUBRC = 0.
        WA_BSET-LSTML = T_LAY-DESCRIPT+0(2).
      endif.
    endif.

    read table IT_BSET  into WL_BSET with key BUKRS  = WA_BSAS-BUKRS
                                              BELNR  = WA_BSAS-BELNR
                                              GJAHR  = WA_BSAS-GJAHR binary search.
    if SY-SUBRC ne 0.
      WA_BSET-BUKRS = WA_BSAS-BUKRS.
      WA_BSET-BELNR = WA_BSAS-BELNR.
      WA_BSET-GJAHR = WA_BSAS-GJAHR.
      WA_BSET-HWBAS = WA_BSAS-DMBTR.
      WA_BSET-FWBAS = WA_BSAS-DMBE2.
      WA_BSET-HWSTE = WA_BSAS-DMBTR.
      WA_BSET-HKONT = WA_BSAS-HKONT.
      WA_BSET-SHKZG = WA_BSAS-SHKZG.
*      WA_BSET-LSTML = ''.
      WA_BSET-MWSKZ = ''.
      append WA_BSET to IT_BSET_AUX.
    endif.
  endloop.

  append lines of IT_BSET_AUX to IT_BSET.

  if P_LAND1 is not initial.
    delete IT_BSET where LSTML not in P_LAND1.
  endif.

  sort IT_BSET by BUKRS BELNR GJAHR.

  if IT_BSET[] is not initial.
*
    select  *
      from T005
      into table IT_T005
        for all entries in IT_BSET
        where LAND1 eq IT_BSET-LSTML.

    select single *
      from T001
      into WL_T001
     where BUKRS = P_BUKRS.


    select single *
      from T005
      into WL_T005
     where LAND1 = WL_T001-LAND1.


    select *
      from T007S
      into corresponding fields of table TL_T007S
         where SPRAS eq SY-LANGU
           and KALSM eq WL_T005-KALSM.

    sort: IT_T005  by LAND1,
          TL_T007S by KALSM MWSKZ.
  endif.

  loop at IT_BSAS into WA_BSAS.
    clear  WA_SAIDA_D.
    if R_TOT = 'X'.
      "Novo
      clear V_HKONT.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          INPUT  = WA_BSAS-HKONT
        importing
          OUTPUT = V_HKONT.
      read table T_VAT201 with key FROM = V_HKONT. "vat
      if SY-SUBRC ne 0.
        read table IT_BSET into WA_BSET with key BUKRS  = WA_BSAS-BUKRS
                                                 BELNR  = WA_BSAS-BELNR
                                                 GJAHR  = WA_BSAS-GJAHR binary search.
        if SY-SUBRC = 0.
          call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
            exporting
              INPUT  = WA_BSET-HKONT
            importing
              OUTPUT = V_HKONT.
        endif.
      else.
        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            INPUT  = WA_BSAS-HKONT
          importing
            OUTPUT = V_HKONT.
      endif.
      if V_HKONT+0(3) = '113'.
        VG_TYPE       = 'To Claim'.
      else.
        VG_TYPE       = 'To Pay'.
      endif.

      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          INPUT  = WA_BSAS-HKONT
        importing
          OUTPUT = WA_BSAS-HKONT.


      loop at IT_BSET  into WA_BSET where BUKRS  = WA_BSAS-BUKRS
                                    and   BELNR  = WA_BSAS-BELNR
                                    and   GJAHR  = WA_BSAS-GJAHR.
        if WA_BSET-SHKZG = 'S'.
          XBASE       = WA_BSET-HWBAS.
          XDMBE2      = WA_BSET-FWBAS.
          XIMPOSTOS   = WA_BSET-HWSTE.
          XBASED      = WA_BSET-FWBAS.
          XIMPOSTOSD  = 0.
        else.
          XBASE       = WA_BSET-HWBAS * -1.
          XDMBE2      = WA_BSET-FWBAS * -1.
          XIMPOSTOS   = WA_BSET-HWSTE * -1.
          XBASED      = WA_BSET-FWBAS * -1.
          XIMPOSTOSD  = 0.
        endif.

        if WA_BSET-MWSKZ  in R_IVA.
          concatenate WA_BSET-MWSKZ '-' into VAL_KIDNO.
        else.
          VAL_KIDNO = WA_BSET-MWSKZ.
        endif.

        read table IT_SAIDA into WA_SAIDA with key LAND1    = WA_BSET-LSTML
                                                   TAX_CODE = VAL_KIDNO
                                                   TYPE     = VG_TYPE.
        if WA_BSAS-STBLG is not initial.
          continue.
        endif.
        if SY-SUBRC ne 0.
          read table IT_T005T into WA_T005T with key LAND1 = WA_BSET-LSTML binary search.
          WA_SAIDA-LAND1      = WA_BSET-LSTML.
          concatenate WA_BSET-LSTML '-' WA_T005T-LANDX into  WA_SAIDA-LANDX.
          WA_SAIDA-TAX_CODE   = VAL_KIDNO.
          WA_SAIDA-TYPE       = VG_TYPE.
          WA_SAIDA-VAT_BASIS  = XBASE.
          WA_SAIDA-RATE       = XRATE.
          WA_SAIDA-DMBE2      = XDMBE2.
          WA_SAIDA-VAT        = XIMPOSTOS.

          WA_SAIDA-VAT_BASISD  = XBASED.
          WA_SAIDA-VATD        = XIMPOSTOSD.

          if XBASE ne 0.
            WA_SAIDA-VATP       = (  XIMPOSTOS / XBASE ) * 100.
          endif.

          WA_SAIDA-QTDE       = 1.
          append WA_SAIDA to IT_SAIDA.
          clear WA_SAIDA.
        else.
          add  XBASE     to WA_SAIDA-VAT_BASIS.
          add  XIMPOSTOS to WA_SAIDA-VAT.

          add  XBASED     to WA_SAIDA-VAT_BASISD.
          add  XIMPOSTOSD to WA_SAIDA-VATD.

          add  XDMBE2    to WA_SAIDA-DMBE2.
          add  1         to WA_SAIDA-QTDE.

          if WA_SAIDA-VAT_BASIS gt 0.
            WA_SAIDA-RATE = WA_SAIDA-DMBE2 / WA_SAIDA-VAT_BASIS.
          endif.
          if  WA_SAIDA-VAT ne 0.
            WA_SAIDA-VATP       = (  WA_SAIDA-VAT / WA_SAIDA-VAT_BASIS ) * 100.
          endif.
          modify IT_SAIDA from WA_SAIDA index SY-TABIX transporting VAT_BASIS VAT VATP QTDE VAT_BASISD VATD .
        endif.
      endloop.
    else.
      "Novo
      clear V_HKONT.
      call function 'CONVERSION_EXIT_ALPHA_INPUT'
        exporting
          INPUT  = WA_BSAS-HKONT
        importing
          OUTPUT = V_HKONT.
      read table T_VAT201 with key FROM = V_HKONT. "vat
      if SY-SUBRC ne 0.
        read table IT_BSET into WA_BSET with key BUKRS  = WA_BSAS-BUKRS
                                                 BELNR  = WA_BSAS-BELNR
                                                 GJAHR  = WA_BSAS-GJAHR binary search.
        if SY-SUBRC = 0.
          call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
            exporting
              INPUT  = WA_BSET-HKONT
            importing
              OUTPUT = V_HKONT.
        endif.
      else.
        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            INPUT  = WA_BSAS-HKONT
          importing
            OUTPUT = V_HKONT.
      endif.

      if V_HKONT+0(3) = '113'.
        VG_TYPE       = 'To Claim'.
      else.
        VG_TYPE       = 'To Pay'.
      endif.

      "nova regra
      clear REVE_BSCHL.
* ---> S4 Migration - 15/06/2023 - MA
*      select single BSCHL
*         into REVE_BSCHL
*        from BSEG
*      where BUKRS  = WA_BSAS-BUKRS
*      and   BELNR  = WA_BSAS-BELNR
*      and   GJAHR  = WA_BSAS-GJAHR
*      and   BSCHL in ( '01', '02', '11', '12', '21', '22', '31', '32' ).

      data:   LT_BSEG type FAGL_T_BSEG.

      types LR_BSCHL_TYPE type range of BSCHL.

      data : LR_BSCHL type LR_BSCHL_TYPE.
      data: IT_BSEG  type FAGL_T_BSEG.

      LR_BSCHL = value LR_BSCHL_TYPE( let S = 'I'
                                          O = 'BT'
                                      in SIGN   = S
                                         OPTION = O
                                         ( LOW = '01' )
                                         ( LOW = '02' )
                                         ( LOW = '11' )
                                         ( LOW = '12' )
                                         ( LOW = '21' )
                                         ( LOW = '22' )
                                         ( LOW = '31' )
                                         ( LOW = '32' ) ).

      call function 'FAGL_GET_BSEG'
        exporting
          I_BUKRS = WA_BSAS-BUKRS
          I_BELNR = WA_BSAS-BELNR
          I_GJAHR = WA_BSAS-GJAHR
        importing
          ET_BSEG = LT_BSEG
        exceptions
          others  = 2.

      delete LT_BSEG where BSCHL not in LR_BSCHL.

      read table LT_BSEG into data(WA_BSEG) index 1.

      if SY-SUBRC eq 0.
        REVE_BSCHL = WA_BSEG-BSCHL.
      endif.
*<--- S4 Migration - 15/06/2023 - MA

      if '31_32' cs REVE_BSCHL.
        VG_TYPE      = 'To Claim'.
      elseif '21_22' cs REVE_BSCHL.
        VG_TYPE       = 'Negative to Claim'.
      elseif '01_02' cs REVE_BSCHL.
        VG_TYPE       = 'To Pay'.
      elseif '11_12' cs REVE_BSCHL.
        VG_TYPE      = 'Negative to Pay'.
      endif.
      "nova regra

      WA_SAIDA_D-TYPE       = VG_TYPE.

      clear: WA_KNA1,WA_BSAK,WA_BSAD,WA_BSET.
      read table IT_BSAK  into WA_BSAK with key BUKRS  = WA_BSAS-BUKRS
                                                BELNR  = WA_BSAS-BELNR
                                                GJAHR  = WA_BSAS-GJAHR binary search.
      if SY-SUBRC = 0.
        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            INPUT  = WA_BSAK-LIFNR
          importing
            OUTPUT = WA_SAIDA_D-HKONT.
        clear WA_KNA1.
        read table IT_LFA1 into WA_KNA1 with key KUNNR = WA_BSAK-LIFNR binary search.
        if SY-SUBRC ne 0.
          read table IT_KNA1 into WA_KNA1 with key KUNNR = WA_BSAK-LIFNR binary search.
        endif.

        WA_BSAD-XBLNR = WA_BSAK-XBLNR.
      else.
        read table IT_BSAD  into WA_BSAD with key BUKRS  = WA_BSAS-BUKRS
                                                  BELNR  = WA_BSAS-BELNR
                                                  GJAHR  = WA_BSAS-GJAHR binary search.
        if SY-SUBRC = 0.
          call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
            exporting
              INPUT  = WA_BSAD-KUNNR
            importing
              OUTPUT = WA_SAIDA_D-HKONT.
          clear WA_KNA1.

          read table IT_KNA1 into WA_KNA1 with key KUNNR = WA_BSAD-KUNNR binary search.
          if SY-SUBRC ne 0.
            read table IT_LFA1 into WA_KNA1 with key KUNNR = WA_BSAD-KUNNR binary search.
          endif.
        else.
          "aqui
          clear: WA_KNA1-NAME1, WA_BSAS_AUX.
          loop at IT_BSAS_AUX into WA_BSAS_AUX where  BUKRS  = WA_BSAS-BUKRS
                                               and    BELNR  = WA_BSAS-BELNR
                                               and    GJAHR  = WA_BSAS-GJAHR.
            if WA_BSAS_AUX-BSCHL ne WA_BSAS-BSCHL.
              call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
                exporting
                  INPUT  = WA_BSAS_AUX-HKONT
                importing
                  OUTPUT = WA_SAIDA_D-HKONT.

              select single TXT20 into WA_KNA1-NAME1
                 from SKAT
                 where SAKNR eq WA_BSAS_AUX-HKONT
                 and   SPRAS eq 'EN'
                 and   KTOPL eq '0050'.

              exit.
            endif.
          endloop.

        endif.
      endif.
      "
      if SY-SUBRC = 0.
        WA_SAIDA_D-NAME1      = WA_KNA1-NAME1.
      endif.


      loop at IT_BSET  into WA_BSET where BUKRS  = WA_BSAS-BUKRS
                                    and   BELNR  = WA_BSAS-BELNR
                                    and   GJAHR  = WA_BSAS-GJAHR.
        WA_SAIDA_D-BELNR      = WA_BSAS-BELNR.
        WA_SAIDA_D-STBLG      = WA_BSAS-STBLG.
        WA_SAIDA_D-BUKRS      = WA_BSAS-BUKRS.
        concatenate WA_BSAS-BUDAT+6(2) '.' WA_BSAS-BUDAT+4(2) '.' WA_BSAS-BUDAT+0(4) into WA_SAIDA_D-BUDAT.
        concatenate WA_BSAS-BLDAT+6(2) '.' WA_BSAS-BLDAT+4(2) '.' WA_BSAS-BLDAT+0(4) into WA_SAIDA_D-BLDAT.
        WA_SAIDA_D-ZUONR      = WA_BSAD-XBLNR.
        WA_SAIDA_D-TAX_CODE   = WA_BSET-MWSKZ.

        WA_SAIDA_DC-TYPE      = VG_TYPE.

        read table IT_T005T into WA_T005T with key LAND1 = WA_BSET-LSTML binary search.
        concatenate WA_BSET-LSTML '-' WA_T005T-LANDX into  WA_SAIDA_D-LANDX.

        call function 'CONVERSION_EXIT_ALPHA_INPUT'
          exporting
            INPUT  = WA_BSAS-HKONT
          importing
            OUTPUT = WA_BSAS-HKONT.

        if WA_BSET-SHKZG = 'H'.
          WA_SAIDA_D-VAT_BASIS  = WA_BSET-HWBAS * -1.
          if WA_BSET-HWBAS ne 0.
            WA_SAIDA_D-RATE       = WA_BSET-FWBAS / WA_BSET-HWBAS.
          endif.
          WA_SAIDA_D-VAT        = WA_BSET-HWSTE * -1.
        else.
          WA_SAIDA_D-VAT_BASIS  = WA_BSET-HWBAS.
          if WA_BSET-HWBAS ne 0.
            WA_SAIDA_D-RATE       = WA_BSET-FWBAS / WA_BSET-HWBAS.
          endif.
          WA_SAIDA_D-VAT        = WA_BSET-HWSTE.
        endif.

        if WA_BSAS-WAERS = 'USD'.
          if WA_BSET-SHKZG = 'H'.
            WA_SAIDA_D-VAT_BASISD  = WA_BSET-FWBAS * -1.
          else.
            WA_SAIDA_D-VAT_BASISD  = WA_BSET-FWBAS.
          endif.
          WA_SAIDA_D-VATD        = 0.
        endif.

        WA_SAIDA_D-VATP = 0.
        if WA_SAIDA_D-VAT_BASIS ne 0.
          WA_SAIDA_D-VATP       = (  WA_SAIDA_D-VAT / WA_SAIDA_D-VAT_BASIS ) * 100.
        endif.

        call function 'CONVERSION_EXIT_ALPHA_OUTPUT'
          exporting
            INPUT  = WA_BSET-HKONT
          importing
            OUTPUT = WA_SAIDA_D-HKONTV.


        read table TL_T007S into WL_T007S with key KALSM = WL_T005-KALSM
                                                   MWSKZ = WA_SAIDA_D-TAX_CODE binary search.
        if SY-SUBRC = 0.
          WA_SAIDA_D-TXT50 = WL_T007S-TEXT1.
        elseif WA_BSET-HKONT is not initial.
          select single TXT20 into WA_SAIDA_D-TXT50
            from SKAT
            where SAKNR eq WA_BSET-HKONT
            and   SPRAS eq 'EN'
            and   KTOPL eq '0050'.
        else.
          if  WA_BSET-MWSKZ  in R_IVA.
            concatenate 'VAT' '0%' into WA_SAIDA_D-TXT50 separated by SPACE.
          endif.
        endif.

        WA_SAIDA_D-ORIGEM = 'SAP'.
        WA_SAIDA_D-SGTXT  =  WA_BSAS-SGTXT.
        loop at IT_BSAS_AUX into WA_BSAS_AUX where  BUKRS  = WA_BSAS-BUKRS
                                             and    BELNR  = WA_BSAS-BELNR
                                             and    GJAHR  = WA_BSAS-GJAHR.
          select single *
            from TBSL
            into WA_TBSL
            where BSCHL = WA_BSAS_AUX-BSCHL
            and   KOART in ( 'K', 'D' ).
          if SY-SUBRC = 0.
            WA_SAIDA_D-SGTXT  =  WA_BSAS_AUX-SGTXT.
            exit.
          endif.
        endloop.

        append WA_SAIDA_D to IT_SAIDA_D.
        if WA_SAIDA_D-STBLG is not initial.
          move-corresponding WA_SAIDA_D to WA_SAIDA_DC.
          clear VG_TYPE.
          WA_SAIDA_DC-BELNR       = WA_SAIDA_D-STBLG.
          WA_SAIDA_DC-STBLG       = WA_SAIDA_D-BELNR.
          WA_SAIDA_DC-VAT_BASIS   = WA_SAIDA_D-VAT_BASIS * -1.
          WA_SAIDA_DC-VAT         = WA_SAIDA_D-VAT * -1.
          WA_SAIDA_DC-VAT_BASISD  = WA_SAIDA_D-VAT_BASISD * -1.
          WA_SAIDA_DC-VATD        = WA_SAIDA_D-VATD * -1.
          WA_SAIDA_DC-TYPE        = VG_TYPE.

          select single BUDAT BLDAT
            from BKPF
            into ( INV_BUDAT, INV_BLDAT )
          where BUKRS = WA_BSAS-BUKRS
          and   BELNR = WA_BSAS-STBLG
          and   GJAHR = WA_BSAS-STJAH.
          concatenate INV_BUDAT+6(2) '.' INV_BUDAT+4(2) '.' INV_BUDAT+0(4) into WA_SAIDA_DC-BUDAT.
          concatenate INV_BLDAT+6(2) '.' INV_BLDAT+4(2) '.' INV_BLDAT+0(4) into WA_SAIDA_DC-BLDAT.

          clear REVE_BSCHL.
* ---> S4 Migration - 15/06/2023 - MA
*          select single BSCHL
*             into REVE_BSCHL
*            from BSEG
*          where BUKRS = WA_BSAS-BUKRS
*          and   BELNR = WA_BSAS-STBLG
*          and   GJAHR = WA_BSAS-STJAH
*          and   BSCHL in ( '01', '02', '11', '12', '21', '22', '31', '32' ).

          data:
            LV_BELNR type BELNR_D,
            LV_BUKRS type BUKRS,
            LV_GJAHR type GJAHR.

          LV_BELNR = WA_BSAS-STBLG.
          LV_GJAHR  = WA_BSAS-STJAH.

          LR_BSCHL = value LR_BSCHL_TYPE( let S = 'I'
                                              O = 'BT'
                                          in SIGN   = S
                                             OPTION = O
                                             ( LOW = '01' )
                                             ( LOW = '02' )
                                             ( LOW = '11' )
                                             ( LOW = '12' )
                                             ( LOW = '21' )
                                             ( LOW = '22' )
                                             ( LOW = '31' )
                                             ( LOW = '32' ) ).

          call function 'FAGL_GET_BSEG'
            exporting
              I_BUKRS = WA_BSAS-BUKRS
              I_BELNR = LV_BELNR
              I_GJAHR = LV_GJAHR
            importing
              ET_BSEG = LT_BSEG
            exceptions
              others  = 2.

          delete LT_BSEG where BSCHL not in LR_BSCHL.

          read table LT_BSEG into WA_BSEG index 1.

          if SY-SUBRC eq 0.
            REVE_BSCHL = WA_BSEG-BSCHL.
          endif.
*<--- S4 Migration - 15/06/2023 - MA

          if '31_32' cs REVE_BSCHL.
            VG_TYPE      = 'To Claim'.
          elseif '21_22' cs REVE_BSCHL.
            VG_TYPE       = 'Negative to Claim'.
          elseif '01_02' cs REVE_BSCHL.
            VG_TYPE       = 'To Pay'.
          elseif '11_12' cs REVE_BSCHL.
            VG_TYPE      = 'Negative to Pay'.
          endif.

          WA_SAIDA_DC-TYPE  = VG_TYPE.


          append WA_SAIDA_DC to IT_SAIDA_D.
        endif.
        clear: WA_SAIDA_D-VAT_BASISD, WA_SAIDA_D-VATD.
      endloop.
    endif.
  endloop.

endform.                    " F_SAIDA
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_IMPRIME_DADOS .
  perform F_ALV_FIELDCAT.

  WA_LAYOUT-ZEBRA      = 'X'.
  WA_LAYOUT-NO_ROWMOVE = 'X'.
  WA_LAYOUT-NO_ROWINS  = 'X'.
  WA_LAYOUT-NO_ROWMARK = SPACE.
  WA_LAYOUT-GRID_TITLE = ''.
  WA_LAYOUT-SEL_MODE   = 'A'.
  WA_LAYOUT-CWIDTH_OPT   = ''.

  call screen 0100.
endform.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module STATUS_0100 output.
  data: FCODE type table of SY-UCOMM.
  refresh: FCODE.

  set pf-status 'F_SET_PF' excluding FCODE.
  set titlebar  'ZFTITLE'.


  if CL_CONTAINER_95 is initial.
    create object CL_CONTAINER_95
      exporting
        SIDE  = '4'
        RATIO = '80'.
  endif.

  if not CL_GRID is initial.

    perform ZF_ALV_HEADER.
    call method CL_GRID->REFRESH_TABLE_DISPLAY.
    if SY-SUBRC <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    endif.

  else.
    create object OBJ_DYNDOC_ID
      exporting
*       STYLE      =
*       BACKGROUND_COLOR =
*       BDS_STYLESHEET =
        NO_MARGINS = 'X'.

    perform ZF_ALV_HEADER .


    if EDITCONTAINER is initial .
      create object EDITCONTAINER
        exporting
          CONTAINER_NAME = 'HEADER'.
    endif .

    call method OBJ_DYNDOC_ID->MERGE_DOCUMENT.

    call method OBJ_DYNDOC_ID->DISPLAY_DOCUMENT
      exporting
        REUSE_CONTROL      = 'X'
        PARENT             = EDITCONTAINER
      exceptions
        HTML_DISPLAY_ERROR = 1.


    create object CL_GRID
      exporting
        I_PARENT = CL_CONTAINER_95.
*         I_PARENT      = CL_CONTAINER
*         I_APPL_EVENTS = 'X'.

    WG_SAVE = 'X'.
    call method CL_GRID->REGISTER_EDIT_EVENT
      exporting
        I_EVENT_ID = CL_GUI_ALV_GRID=>MC_EVT_ENTER.

    WA_STABLE-ROW        = C_X.
    WG_X_VARIANT-REPORT  = SY-REPID.

    if R_TOT = 'X'.
      call method CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        exporting
          IS_VARIANT      = WG_X_VARIANT
          IS_LAYOUT       = WA_LAYOUT
          I_SAVE          = WG_SAVE
        changing
          IT_FIELDCATALOG = IT_FIELDCAT[]
          IT_SORT         = I_SORT[]
          IT_OUTTAB       = IT_SAIDA[].
    else.
      call method CL_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        exporting
          IS_VARIANT      = WG_X_VARIANT
          IS_LAYOUT       = WA_LAYOUT
          I_SAVE          = WG_SAVE
        changing
          IT_FIELDCATALOG = IT_FIELDCAT[]
          IT_SORT         = I_SORT[]
          IT_OUTTAB       = IT_SAIDA_D[].
    endif.


    create object EVENT_RECEIVER.
    set handler EVENT_RECEIVER->CATCH_HOTSPOT           for CL_GRID.



  endif.

endmodule.                 " STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form ZF_ALV_HEADER .
  data:   WL_DATA(10),
                WL_HORA(8),
                WL_LINHA(60),
                WL_TEXT type SDYDO_TEXT_ELEMENT.

  if R_TOT = 'X'.
    WL_TEXT = 'Tax Reports – VAT  (Totalizer)'.
  else.
    WL_TEXT = 'Tax Reports – VAT  (Detailed)'.
  endif.

  call method OBJ_DYNDOC_ID->ADD_TEXT
    exporting
      TEXT         = WL_TEXT
      SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>EXTRA_LARGE
      SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.

  call method OBJ_DYNDOC_ID->NEW_LINE.

  clear WL_TEXT.
  call method OBJ_DYNDOC_ID->ADD_TEXT
    exporting
      TEXT         = WL_TEXT "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.


  concatenate 'Company Code  :' P_BUKRS-LOW
     into WL_LINHA separated by SPACE.

  WL_TEXT = WL_LINHA.
  call method OBJ_DYNDOC_ID->NEW_LINE.

  call method OBJ_DYNDOC_ID->ADD_TEXT
    exporting
      TEXT         = WL_TEXT "WL_LINHA
*     SAP_STYLE    = CL_DD_AREA=>HEADING
      SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.


  if P_BUDAT is not initial.
    concatenate P_BUDAT-LOW+6(2) P_BUDAT-LOW+4(2) P_BUDAT-LOW+0(4) into  WL_DATA separated by '.'.
    "WL_DATA = P_BUDAT-LOW.
    if P_BUDAT-HIGH is initial.
      concatenate 'Posting Date  :' WL_DATA
      into WL_LINHA separated by SPACE.
    else.
      concatenate 'Posting Date :' WL_DATA  into WL_LINHA separated by SPACE.
      concatenate P_BUDAT-HIGH+6(2) P_BUDAT-HIGH+4(2) P_BUDAT-HIGH+0(4) into  WL_DATA separated by '.'.
      "WL_DATA = P_BUDAT-HIGH.
      concatenate WL_LINHA 'à' WL_DATA  into WL_LINHA separated by SPACE.
    endif.
    WL_TEXT = WL_LINHA.
    call method OBJ_DYNDOC_ID->NEW_LINE.

    call method OBJ_DYNDOC_ID->ADD_TEXT
      exporting
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  endif.

  if P_BLDAT is not initial.
    concatenate P_BLDAT-LOW+6(2) P_BLDAT-LOW+4(2) P_BLDAT-LOW+0(4) into  WL_DATA separated by '.'.
    "WL_DATA = P_BUDAT-LOW.
    if P_BLDAT-HIGH is initial.
      concatenate 'Document Date  :' WL_DATA
      into WL_LINHA separated by SPACE.
    else.
      concatenate 'Document Date :' WL_DATA  into WL_LINHA separated by SPACE.
      concatenate P_BLDAT-HIGH+6(2) P_BLDAT-HIGH+4(2) P_BLDAT-HIGH+0(4) into  WL_DATA separated by '.'.
      "WL_DATA = P_BUDAT-HIGH.
      concatenate WL_LINHA 'à' WL_DATA  into WL_LINHA separated by SPACE.
    endif.
    WL_TEXT = WL_LINHA.
    call method OBJ_DYNDOC_ID->NEW_LINE.

    call method OBJ_DYNDOC_ID->ADD_TEXT
      exporting
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*         SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT.
  endif.


  if P_LAND1-LOW is not initial.
    select single LAND1 LANDX
          from T005T
          into WA_T005T
          where LAND1 eq P_LAND1-LOW
          and SPRAS = 'EN'.

    concatenate 'Country :' P_LAND1-LOW '-' WA_T005T-LANDX
         into WL_LINHA separated by SPACE.

    WL_TEXT = WL_LINHA.
    call method OBJ_DYNDOC_ID->NEW_LINE.

    call method OBJ_DYNDOC_ID->ADD_TEXT
      exporting
        TEXT         = WL_TEXT "WL_LINHA
*       SAP_STYLE    = CL_DD_AREA=>HEADING
        SAP_FONTSIZE = CL_DD_AREA=>LIST_NORMAL.
*        SAP_COLOR    = CL_DD_AREA=>LIST_HEADING_INT
  endif.

endform.                    " ZF_ALV_HEADER
*&---------------------------------------------------------------------*
*&      Form  F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
form F_ALV_FIELDCAT .
  data I type I.

  if R_TOT = 'X'.
    WA_AFIELD-TABNAME     = 'IT_SAIDA'.
    WA_AFIELD-COLDDICTXT = 'M'.
    WA_AFIELD-SELDDICTXT = 'M'.
    WA_AFIELD-TIPDDICTXT = 'M'.
    WA_AFIELD-COL_OPT = 'X'.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'LANDX'.
    WA_AFIELD-SCRTEXT_S = 'Country'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 15.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'TAX_CODE'.
    WA_AFIELD-SCRTEXT_S = 'VAT code'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'TYPE'.
    WA_AFIELD-SCRTEXT_S = 'Type'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'VAT_BASISD'.
    WA_AFIELD-SCRTEXT_S = 'VAT Basis USD'.
    WA_AFIELD-SCRTEXT_L = 'VAT Basis USD'.
    WA_AFIELD-SCRTEXT_M = 'VAT Basis USD'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 20.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'VAT_BASIS'.
    WA_AFIELD-SCRTEXT_S = 'VAT Basis EUR'.
    WA_AFIELD-SCRTEXT_L = 'VAT Basis EUR'.
    WA_AFIELD-SCRTEXT_M = 'VAT Basis EUR'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 20.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'VAT'.
    WA_AFIELD-SCRTEXT_S = 'VAT EUR'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 15.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'RATE'.
    WA_AFIELD-SCRTEXT_S = 'Rate'.
    WA_AFIELD-SCRTEXT_L = 'Exchange Rate'.
    WA_AFIELD-SCRTEXT_M = 'Exchange Rate'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 12.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'VATP'.
    WA_AFIELD-SCRTEXT_S = 'VAT %'.
    WA_AFIELD-SCRTEXT_L = 'Calculated VAT %'.
    WA_AFIELD-SCRTEXT_M = 'Calculated VAT %'.
    WA_AFIELD-OUTPUTLEN     = 15.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'QTDE'.
    WA_AFIELD-SCRTEXT_S = 'Quantity'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 15.
    append WA_AFIELD to IT_FIELDCAT.

  else.
    WA_AFIELD-TABNAME     = 'IT_SAIDA_D'.
    WA_AFIELD-COLDDICTXT = 'M'.
    WA_AFIELD-SELDDICTXT = 'M'.
    WA_AFIELD-TIPDDICTXT = 'M'.
    WA_AFIELD-COL_OPT = 'X'.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'HKONT'.
    WA_AFIELD-SCRTEXT_S = 'Account'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 10.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'NAME1'.
    WA_AFIELD-SCRTEXT_S = 'Name'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-OUTPUTLEN     = 35.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'BELNR'.
    WA_AFIELD-SCRTEXT_S = 'Doc Number'.
    WA_AFIELD-SCRTEXT_L = 'Document Number'.
    WA_AFIELD-SCRTEXT_M = 'Document Number'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 12.
    WA_AFIELD-HOTSPOT       = 'X'.
    append WA_AFIELD to IT_FIELDCAT.


    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'HKONTV'.
    WA_AFIELD-SCRTEXT_S = 'D/C SAP Account'.
    WA_AFIELD-SCRTEXT_L = 'D/C SAP Account'.
    WA_AFIELD-SCRTEXT_M = 'D/C SAP Account'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 15.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'TXT50'.
    WA_AFIELD-SCRTEXT_S = 'Description'.
    WA_AFIELD-SCRTEXT_L = 'Description'.
    WA_AFIELD-SCRTEXT_M = 'Description'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 15.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'STBLG'.
    WA_AFIELD-SCRTEXT_S = 'Rev. Document'.
    WA_AFIELD-SCRTEXT_L = 'Reverse Document'.
    WA_AFIELD-SCRTEXT_M = 'Reverse Document'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 12.
*    WA_AFIELD-HOTSPOT       = 'X'.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'BUDAT'.
    WA_AFIELD-SCRTEXT_S = 'Posting Date'.
    WA_AFIELD-SCRTEXT_L = 'Posting Date'.
    WA_AFIELD-SCRTEXT_M = 'Posting Date'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 12.
    append WA_AFIELD to IT_FIELDCAT.


    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'BLDAT'.
    WA_AFIELD-SCRTEXT_S = 'Document Date'.
    WA_AFIELD-SCRTEXT_L = 'Document Date'.
    WA_AFIELD-SCRTEXT_M = 'Document Date'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 12.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'ZUONR'.
    WA_AFIELD-SCRTEXT_S = 'Parcel'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 12.
    append WA_AFIELD to IT_FIELDCAT.

    "XBLNR
    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'XBLNR'.
    WA_AFIELD-SCRTEXT_S = 'invoice'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 12.
    append WA_AFIELD to IT_FIELDCAT.
    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'TAX_CODE'.
    WA_AFIELD-SCRTEXT_S = 'VAT code'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 10.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'LANDX'.
    WA_AFIELD-SCRTEXT_S = 'Country'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 15.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'TYPE'.
    WA_AFIELD-SCRTEXT_S = 'Type'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'VAT_BASISD'.
    WA_AFIELD-SCRTEXT_S = 'VAT Basis USD'.
    WA_AFIELD-SCRTEXT_L = 'VAT Basis USD'.
    WA_AFIELD-SCRTEXT_M = 'VAT Basis USD'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 20.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'VAT_BASIS'.
    WA_AFIELD-SCRTEXT_S = 'VAT Basis EUR'.
    WA_AFIELD-SCRTEXT_L = 'VAT Basis EUR'.
    WA_AFIELD-SCRTEXT_M = 'VAT Basis EUR'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 20.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'VAT'.
    WA_AFIELD-SCRTEXT_S = 'VAT EUR'.
    WA_AFIELD-SCRTEXT_L = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-SCRTEXT_M = WA_AFIELD-SCRTEXT_S.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 12.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'RATE'.
    WA_AFIELD-SCRTEXT_S = 'Rate'.
    WA_AFIELD-SCRTEXT_L = 'Exchange Rate'.
    WA_AFIELD-SCRTEXT_M = 'Exchange Rate'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 12.
    append WA_AFIELD to IT_FIELDCAT.
    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'VATP'.
    WA_AFIELD-SCRTEXT_S = 'VAT %'.
    WA_AFIELD-SCRTEXT_L = 'Calculated VAT %'.
    WA_AFIELD-SCRTEXT_M = 'Calculated VAT %'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 15.
    append WA_AFIELD to IT_FIELDCAT.

    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'ORIGEM'.
    WA_AFIELD-SCRTEXT_S = 'System'.
    WA_AFIELD-SCRTEXT_L = 'System'.
    WA_AFIELD-SCRTEXT_M = 'System'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 15.
    append WA_AFIELD to IT_FIELDCAT.


    I = I + 1.
    clear WA_AFIELD.
    WA_AFIELD-COL_POS       = I.
    WA_AFIELD-FIELDNAME     = 'SGTXT'.
    WA_AFIELD-SCRTEXT_S = 'Text'.
    WA_AFIELD-SCRTEXT_L = 'Text'.
    WA_AFIELD-SCRTEXT_M = 'Text'.
    WA_AFIELD-EDIT          = ''.
    WA_AFIELD-KEY           = ''.
    WA_AFIELD-OUTPUTLEN     = 25.
    append WA_AFIELD to IT_FIELDCAT.
  endif.

endform.                    " F_ALV_FIELDCAT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module USER_COMMAND_0100 input.
  case SY-UCOMM.
    when 'BACK' or 'UP'.
      refresh IT_SAIDA.
      refresh IT_SAIDA_D.
      call method CL_GRID->REFRESH_TABLE_DISPLAY.
      leave to screen 0.
    when 'CANCEL'.
      leave program.
  endcase.
endmodule.                 " USER_COMMAND_0100  INPUT
