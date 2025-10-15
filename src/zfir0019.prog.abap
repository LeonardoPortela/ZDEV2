*----------------------------------------------------------------------*
*                                                                      *
* Programa   : ZFIR0019                                                *
* Descrição  : Resumo Ordem de Vendas e Contas a REceber               *
* Módulo     : FI                                Transação: ZFIS19     *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Paulo Bonetti                          Data: 15/12/2010 *
* Observações: Desenvolvimento inicial do Programa                     *
* Alteração  : Camila Brand                          Data: 22/06/2011  *
*Observações: Conforme solicitado chamado: 43139
*----------------------------------------------------------------------*

REPORT  zfir0019.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon, slis.

*----------------------------------------------------------------------*
* TABLES
*----------------------------------------------------------------------*
TABLES: vbak ,
        vbap ,
        tcurr,
        vbfa ,
        vbkd .

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:

  "     Dados Ordem de Venda (Cabeçalho)
  BEGIN OF ty_vbak             ,
    vbeln TYPE vbak-vbeln,
    erdat TYPE vbak-erdat,
    auart TYPE vbak-auart,
    audat TYPE vbak-audat,
    vkorg TYPE vbak-vkorg,
    vkbur TYPE vbak-vkbur,
    vtweg TYPE vbak-vtweg,
    spart TYPE vbak-spart,
    kunnr TYPE vbak-kunnr,
    waerk TYPE vbak-waerk,
    knumv TYPE vbak-knumv,
    vgbel TYPE vbak-vgbel,
    kvgr2 TYPE vbak-kvgr2,

  END   OF ty_vbak             ,

  " Dados Contratos
  BEGIN OF ty_vbfa_cont          ,
    vbeln   TYPE vbfa-vbeln,
    vbelv   TYPE vbfa-vbeln,
    vbtyp_v TYPE vbfa-vbtyp_v,
  END   OF ty_vbfa_cont          ,

  "     Dados Ordem de Venda (Item)
  BEGIN OF ty_vbap             ,
    vbeln TYPE vbap-vbeln,
    posnr TYPE vbap-posnr,
    matnr TYPE vbap-matnr,
    werks TYPE vbap-werks,
    charg TYPE vbap-charg,
  END   OF ty_vbap             ,

  BEGIN OF ty_konv             ,
    knumv TYPE konv-knumv,
    kschl TYPE konv-kschl,
    kwert TYPE konv-kwert,
    kbetr TYPE konv-kbetr,
  END   OF ty_konv             ,

  BEGIN OF ty_vbep             ,
    vbeln TYPE vbep-vbeln,
    posnr TYPE vbep-posnr,
    etenr TYPE vbep-etenr,
    wmeng TYPE vbep-wmeng,
  END   OF ty_vbep             ,

  BEGIN OF ty_kna1             ,
    kunnr TYPE kna1-kunnr,
    name1 TYPE kna1-name1,
  END   OF ty_kna1             ,

  BEGIN OF ty_makt             ,
    matnr TYPE makt-matnr,
    maktx TYPE makt-maktx,
  END   OF ty_makt             ,

  BEGIN OF ty_vbfa             ,
    vbelv TYPE vbfa-vbelv,
    rfmng TYPE vbfa-rfmng,
  END   OF ty_vbfa             ,

  BEGIN OF ty_vbfa_ar          ,
    vbelv   TYPE vbfa-vbelv,
    posnv   TYPE vbfa-posnv,
    vbeln_  TYPE vbfa-vbeln,
    posnn   TYPE vbfa-posnn,
    vbtyp_n TYPE vbfa-vbtyp_n,
    erdat   TYPE vbfa-erdat,
    bukrs   TYPE bsad-bukrs,
    kunnr   TYPE bsad-kunnr,
    vbeln   TYPE bsad-vbeln,
    dmbtr   TYPE bsad-dmbtr,
    dmbe2   TYPE bsad-dmbe2,
    shkzg   TYPE bsad-shkzg,
    augdt   TYPE bsad-augdt,
    rebzg   TYPE bsad-rebzg,
    augbl   TYPE bsad-augbl,
    belnr   TYPE bsad-belnr,
    gjahr   TYPE bsad-gjahr,
  END   OF ty_vbfa_ar          ,

  BEGIN OF ty_bsid_aux         ,
    bukrs TYPE bsid-bukrs,
    belnr TYPE bsid-belnr,
    rebzg TYPE bsid-rebzg,
    dmbtr TYPE bsid-dmbtr,
    dmbe2 TYPE bsid-dmbe2,
    gjahr TYPE bsid-gjahr,
  END   OF ty_bsid_aux         ,

  BEGIN OF ty_adto             ,
    bukrs TYPE bsid-bukrs,
    kunnr TYPE bsid-kunnr,
    belnr TYPE bsid-belnr,
    buzei TYPE bsid-buzei,
    gjahr TYPE bsid-gjahr,
    dmbtr TYPE bsid-dmbtr,
    dmbe2 TYPE bsid-dmbe2,
    vbel2 TYPE bsid-vbel2,
  END   OF ty_adto             ,

  BEGIN OF ty_bseg             ,
    hzuon TYPE bseg-hzuon,
    belnr TYPE bseg-belnr,
    buzei TYPE bseg-buzei,
    gjahr TYPE bseg-gjahr,
  END   OF ty_bseg             ,

  BEGIN OF ty_vbfa_ar_pg       ,
    vbelv   TYPE vbfa-vbelv,
    posnv   TYPE vbfa-posnv,
    vbeln_  TYPE vbfa-vbeln,
    posnn   TYPE vbfa-posnn,
    vbtyp_n TYPE vbfa-vbtyp_n,
    erdat   TYPE vbfa-erdat,
    bukrs   TYPE bsad-bukrs,
    kunnr   TYPE bsad-kunnr,
    vbeln   TYPE bsad-vbeln,
    dmbtr   TYPE bsad-dmbtr,
    dmbe2   TYPE bsad-dmbe2,
    shkzg   TYPE bsad-shkzg,
    augdt   TYPE bsad-augdt,
    augbl   TYPE bsad-augbl,
    rebzg   TYPE bsad-rebzg,
    belnr   TYPE bsad-belnr,
    gjahr   TYPE bsad-gjahr,
  END   OF ty_vbfa_ar_pg       ,

  BEGIN OF ty_bsid             ,
    bukrs TYPE bsid-bukrs,
    kunnr TYPE bsid-kunnr,
    vbeln TYPE bsid-vbeln,
    dmbtr TYPE bsid-dmbtr,
    dmbe2 TYPE bsid-dmbe2,
  END   OF ty_bsid             ,

  BEGIN OF ty_bsid_ac          ,
    vbel2 TYPE bsid-vbel2,
    blart TYPE bsid-blart,
    budat TYPE bsid-budat,
    dmbtr TYPE bsid-dmbtr,
    dmbe2 TYPE bsid-dmbe2,
    shkzg TYPE bsid-shkzg,
    bukrs TYPE bsid-bukrs,
    belnr TYPE bsid-belnr,
    gjahr TYPE bsid-gjahr,
    augbl TYPE bsid-augbl,
  END   OF ty_bsid_ac          ,

  BEGIN OF ty_bsad_ac           ,
    vbel2 TYPE bsad-vbel2,
    blart TYPE bsad-blart,
    budat TYPE bsad-budat,
    dmbtr TYPE bsad-dmbtr,
    dmbe2 TYPE bsad-dmbe2,
    shkzg TYPE bsad-shkzg,
    bukrs TYPE bsad-bukrs,
    belnr TYPE bsad-belnr,
    gjahr TYPE bsad-gjahr,
    augbl TYPE bsad-augbl,
  END   OF ty_bsad_ac          ,

  BEGIN OF ty_tcurr            ,
    gdatu TYPE tcurr-gdatu,
    ukurs TYPE tcurr-ukurs,
  END   OF ty_tcurr            ,

  BEGIN OF ty_t001w            ,
    werks TYPE t001w-werks,
    name1 TYPE t001w-name1,
  END   OF ty_t001w            ,

  BEGIN OF ty_tvakt            ,
    auart TYPE tvakt-auart,
    bezei TYPE tvakt-bezei,
  END   OF ty_tvakt            ,

  BEGIN OF ty_vbfa_ini          ,
    vbelv   TYPE vbfa-vbelv,
    vbtyp_n TYPE vbfa-vbtyp_n,
    erdat   TYPE vbfa-erdat,
  END   OF ty_vbfa_ini          ,

  BEGIN OF ty_vbkd             ,
    vbeln TYPE vbkd-vbeln,
    posnr TYPE vbkd-posnr,
    inco1 TYPE vbkd-inco1,
    bstkd TYPE vbkd-bstkd,
  END OF ty_vbkd               ,

  BEGIN OF ty_vbfa_ov           ,
    vbeln   TYPE vbfa-vbeln,
    vbelv   TYPE vbfa-vbelv,
    vbtyp_v TYPE vbfa-vbtyp_v,
  END   OF ty_vbfa_ov           ,


  BEGIN OF ty_saida                ,
    vbeln         TYPE vbak-vbeln,
    vbelv         TYPE vbfa-vbelv,
    erdat         TYPE vbak-erdat,
    auart         TYPE vbak-auart,
    audat         TYPE vbak-audat,
    vkorg         TYPE vbak-vkorg,
    vkbur         TYPE vbak-vkbur,
    vtweg         TYPE vbak-vtweg,
    spart         TYPE vbak-spart,
    name1         TYPE kna1-name1,
    waerk         TYPE vbak-waerk,
    maktx         TYPE makt-maktx,
    werks         TYPE vbap-werks,
    charg         TYPE vbap-charg,
    wmeng         TYPE vbep-wmeng,
    xtrant_atu    TYPE vbfa-rfmng , "CSB
    xtratu        TYPE vbfa-rfmng,
    xtrant        TYPE vbfa-rfmng,
    fatura        TYPE vbfa-vbeln,
    dt_fat        TYPE vbfa-erdat,
    kunnr         TYPE vbak-kunnr,
    matnr         TYPE vbap-matnr,
    kwert         TYPE konv-kwert,
    tot_rs        TYPE konv-kwert,
    tot_us        TYPE konv-kwert,
    tot_liq_rs    TYPE konv-kwert,
    tot_liq_us    TYPE konv-kwert,
    tot_cif_rs    TYPE konv-kwert,
    tot_cif_us    TYPE konv-kwert,
    sld_ov        TYPE vbep-wmeng,
    xfrreal_1     TYPE bsid-dmbtr,
    xfrdolar_1    TYPE bsid-dmbe2,
    xfrreal_2     TYPE bsid-dmbtr,
    xfrdolar_2    TYPE bsid-dmbe2,
    yfrreal_2     TYPE bsid-dmbtr,
    yfrdolar_2    TYPE bsid-dmbe2,
    sld_rec_rs    TYPE konv-kwert,
    sld_rec_us    TYPE konv-kwert,
    yfrreal_1     TYPE konv-kwert,
    yfrdolar_1    TYPE konv-kwert,
    fat_rec_r     TYPE konv-kwert,
    fat_rec_d     TYPE konv-kwert,
    centro        TYPE t001w-name1,
    kbetr         TYPE konv-kbetr,
    tipo_frete    TYPE vbkd-inco1,
    bstkd         TYPE vbkd-bstkd,
    bezei         TYPE tvakt-bezei,
    dt_rem        TYPE vbfa-erdat,
    vgbel         TYPE vbak-vgbel,
    vlr_fat_per_r TYPE bsid-dmbtr,
    vlr_fat_per_d TYPE bsid-dmbtr,
    vlr_fat_acum  TYPE bsid-dmbtr,
    nro_sol_ov    TYPE zsdt0053-nro_sol_ov,
    formula2      TYPE zsdt0059-formula2,
    navio         TYPE zsdt0053-navio, " Incluido wsb

  END OF ty_saida                  .


TYPES: BEGIN OF ty_estrutura.
         INCLUDE TYPE slis_fieldcat_main.
         INCLUDE TYPE slis_fieldcat_alv_spec.
TYPES: END OF ty_estrutura.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*

DATA: it_vbak           TYPE TABLE OF ty_vbak,
      it_vbakii         TYPE TABLE OF ty_vbak,
      it_vbfa_cont      TYPE TABLE OF ty_vbfa_cont,
      it_vbap           TYPE TABLE OF ty_vbap,
      it_konv           TYPE TABLE OF ty_konv,
      it_vbep           TYPE TABLE OF ty_vbep,
      it_kna1           TYPE TABLE OF ty_kna1,
      it_kna1_1         TYPE TABLE OF ty_kna1,
      it_makt           TYPE TABLE OF ty_makt,
      it_vbfa_ov        TYPE TABLE OF ty_vbfa_ov,
      it_vbfa_dev       TYPE TABLE OF ty_vbfa_ov,
      it_vbfa_ant       TYPE TABLE OF vbfa,
      it_vbfa_atu       TYPE TABLE OF vbfa,
      it_vbfa_ant_atu   TYPE TABLE OF vbfa         , "CSB
      it_vbfa_ar_pg_atu TYPE TABLE OF ty_vbfa_ar_pg,
      it_vbfa_ar_pg_ant TYPE TABLE OF ty_vbfa_ar_pg,
      it_vbfa_rec       TYPE TABLE OF ty_vbfa_ar_pg,
      it_bsid_atu       TYPE TABLE OF ty_bsid_aux,
      it_bsid_fat       TYPE TABLE OF ty_bsid_aux,
      it_bkpf_aux       TYPE TABLE OF bkpf,
      it_bsad_val       TYPE TABLE OF bsad,
      it_bsad_canc      TYPE TABLE OF bsad,
      it_bsid_ant1      TYPE TABLE OF ty_bsid_aux,
      it_vbfa_ar_per    TYPE TABLE OF ty_vbfa_ar,
      it_vbfa_ar_ant    TYPE TABLE OF ty_vbfa_ar,
      it_vbfa_ar_atu    TYPE TABLE OF ty_vbfa_ar,
      it_tcurr          TYPE TABLE OF ty_tcurr,
      it_bsid           TYPE TABLE OF ty_bsid,
      it_bsid_ac        TYPE TABLE OF ty_bsid_ac,
      it_bsid_fat_p     TYPE TABLE OF ty_bsid_ac,
      it_bsad_ac        TYPE TABLE OF ty_bsad_ac,
      it_bsad_fat_p     TYPE TABLE OF ty_bsad_ac,
      it_bsid_ant       TYPE TABLE OF ty_bsid,
      it_t001w          TYPE TABLE OF ty_t001w,
      it_vbfa_ini       TYPE TABLE OF ty_vbfa_ini,
      it_adto           TYPE TABLE OF ty_adto,
      it_bseg           TYPE TABLE OF ty_bseg,
      it_saida          TYPE TABLE OF ty_saida,
      it_vbkd           TYPE TABLE OF ty_vbkd,
      it_tvakt          TYPE TABLE OF ty_tvakt,
      it_zsdt0059       TYPE TABLE OF zsdt0059,
      it_zsdt0053       TYPE TABLE OF zsdt0053,
      it_setleaf        LIKE TABLE OF setleaf INITIAL SIZE 0 WITH HEADER LINE,
      t_bdc             TYPE TABLE OF bdcdata WITH HEADER LINE INITIAL SIZE 0,
      t_messtab         TYPE TABLE OF bdcmsgcoll.

*&---------------------------------------------------------------------*
*& Ranges
*&---------------------------------------------------------------------*
RANGES: rg_gjahr FOR bsad-gjahr.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*

DATA:wa_vbak           TYPE ty_vbak,
     wa_vbfa_cont      TYPE ty_vbfa_cont,
     wa_vbap           TYPE ty_vbap,
     wa_konv           TYPE ty_konv,
     wa_vbep           TYPE ty_vbep,
     wa_kna1           TYPE ty_kna1,
     wa_kna1_1         TYPE ty_kna1,
     wa_makt           TYPE ty_makt,
     wa_vbfa_ant       TYPE vbfa,
     wa_vbfa_atu       TYPE vbfa,
     wa_vbfa_ant_atu   TYPE vbfa,
     wa_vbfa_ov        TYPE ty_vbfa_ov,
     wa_vbfa_rec       TYPE ty_vbfa_ar_pg,
     wa_vbfa_ar_pg_atu TYPE ty_vbfa_ar_pg,
     wa_vbfa_ar_pg_ant TYPE ty_vbfa_ar_pg,
     wa_bsid_aux       TYPE ty_bsid_aux,
     wa_bsid_ac        TYPE ty_bsid_ac,
     wa_bsid_fat_p     TYPE ty_bsid_ac,
     wa_bsad_ac        TYPE ty_bsad_ac,
     wa_bsad_fat_p     TYPE ty_bsad_ac,
     wa_bsid_fat       TYPE ty_bsid_aux,
     wa_bkpf_aux       TYPE bkpf,
     wa_bsad_val       TYPE bsad,
     wa_bsad_canc      TYPE bsad,
     wa_vbfa_ar_per    TYPE ty_vbfa_ar,
     wa_vbfa_ar_ant    TYPE ty_vbfa_ar,
     wa_vbfa_ar_atu    TYPE ty_vbfa_ar,
     wa_tcurr          TYPE ty_tcurr,
     wa_t001w          TYPE ty_t001w,
     wa_vbfa_ini       TYPE ty_vbfa_ini,
     wa_saida          TYPE ty_saida,
     wa_vbkd           TYPE ty_vbkd,
     wa_tvakt          TYPE ty_tvakt,
     wa_adto           TYPE ty_adto,
     wa_bseg           TYPE ty_bseg,
     wa_setleaf        TYPE setleaf,
     wa_zsdt0053       TYPE zsdt0053,
     wa_zsdt0059       TYPE zsdt0059,
     wa_cont           TYPE REF TO cl_gui_custom_container , " Objeto Container
     wa_alv            TYPE REF TO cl_gui_alv_grid, " Objeto ALV
     wa_layout         TYPE lvc_s_layo           . " Layout da Lista / Fim do DATA


*&---------------------------------------------------------------------*
*& Estrutura ALV
*&---------------------------------------------------------------------*

DATA: it_fcat   TYPE TABLE OF ty_estrutura, " lvc_s_fcat,
      s_variant TYPE disvariant           , " Tabela Estrutura colunas relatorio
      t_top     TYPE slis_t_listheader,
      xs_events TYPE slis_alv_event,
      events    TYPE slis_t_event,
      t_print   TYPE slis_print_alv,
      v_report  LIKE sy-repid,
      t_sort    TYPE slis_t_sortinfo_alv WITH HEADER LINE.


*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: p_vbeln  FOR vbak-vbeln              ,
                  p_vkorg  FOR vbak-vkorg  OBLIGATORY  ,"Organização de Vendas
                  p_vkbur  FOR vbak-vkbur              ,"Escritorio de vendas
                  p_spart  FOR vbak-spart              ,"Setor de Atividade
                  p_vtweg  FOR vbak-vtweg              ,"Canal de distribuição
                  p_charg  FOR vbap-charg              ,"Safra   *
                  p_werks  FOR vbap-werks              ,"Centro  *
                  p_kunnr  FOR vbak-kunnr              ,"Cliente
                  p_matnr  FOR vbap-matnr              ,"Material *
                  p_waerk  FOR vbak-waerk              ,"Moeda
                  p_auart  FOR vbak-auart              ,"Tipo de Ordem de Venda
                  p_bstkd  FOR vbkd-bstkd              ,"Numero do Pedido
                  p_kurst  FOR tcurr-kurst DEFAULT 'B' OBLIGATORY,"Categoria Moeda
                  p_erdat  FOR vbfa-erdat  OBLIGATORY NO-EXTENSION ,"Período
                  p_period FOR vbak-erdat  OBLIGATORY  .
SELECTION-SCREEN: END OF BLOCK b1.

SELECTION-SCREEN: BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-038.
  PARAMETERS: p_varia LIKE disvariant-variant.
SELECTION-SCREEN: END OF BLOCK b2.

*Início Alteração Ricardo Furst.
*---------------------------------------------------------------------*
* Event selection-screen on value-request for p_var
*---------------------------------------------------------------------*
DATA: vg_repid   LIKE sy-repid,
      vg_variant TYPE disvariant.

INITIALIZATION.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_varia.

*  IF NOT p_varia IS INITIAL.
  vg_repid          = sy-repid.
  s_variant-report = vg_repid.

  IF ( NOT p_varia IS INITIAL ).
    vg_variant-variant = p_varia.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant    = s_variant
      i_save        = 'A'
    IMPORTING
      es_variant    = s_variant
    EXCEPTIONS
      not_found     = 1
      program_error = 2
      OTHERS        = 3.


  IF ( sy-subrc NE 0 ).
    MESSAGE s000(z01) WITH 'Não existe variante'.
    STOP.
  ELSE.
    MOVE s_variant-variant TO p_varia.
  ENDIF.
*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  DATA : mes_inicial TYPE c LENGTH 2,
         mes_final   TYPE c LENGTH 2,
         ano_inicial TYPE c LENGTH 4,
         ano_final   TYPE c LENGTH 4.

  REFRESH: rg_gjahr.

  rg_gjahr-sign   = 'I'.
  rg_gjahr-option = 'EQ'.
  rg_gjahr-low    = p_erdat-low(4).
  APPEND rg_gjahr.
  CLEAR: rg_gjahr.

  IF p_erdat-high IS NOT INITIAL
  AND p_erdat-high NE p_erdat-low.
    rg_gjahr-sign   = 'I'.
    rg_gjahr-option = 'EQ'.
    rg_gjahr-low    = p_erdat-high(4).
    APPEND rg_gjahr.
    CLEAR: rg_gjahr.

  ENDIF.
*  CALL FUNCTION 'CACS_DATE_GET_YEAR_MONTH'
*    EXPORTING
*      i_date  = p_erdat-low
*    IMPORTING
*      e_month = mes_inicial
*      e_year  = ano_inicial.
*
*  CALL FUNCTION 'CACS_DATE_GET_YEAR_MONTH'
*    EXPORTING
*      i_date  = p_erdat-high
*    IMPORTING
*      e_month = mes_final
*      e_year  = ano_final.

*  IF mes_inicial NE mes_final OR ano_inicial NE ano_final.
*    MESSAGE i000(z01) WITH 'O periodo informado deve estar dentro '
*                           'de um único mesmo mês !'  .
*    STOP.
*  ENDIF.

  IF p_erdat-low < p_period-low .
    MESSAGE i000(z01) WITH 'Periodo Remessa: A Data inicial informada não pode '
                           'ser menor que a data inicial informada no Campo '
                           'Periodo da Ordem de Venda !'        .
    STOP.
  ENDIF.

  IF p_period-low > p_erdat-low .
    MESSAGE i000(z01) WITH 'Periodo Ordem de Venda : A Data inicial informada não pode '
                           'ser maior que a data inicial informada no Campo '
                           'Periodo da Remessa !'        .
    STOP.
  ENDIF.

  PERFORM:  f_iniciar_variaves                      ,
            f_seleciona_dados                       , " Form seleciona dados
            f_saida                                 , " Form de saida
            f_imprime_dados                                   . " Form ALV

END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       Form para selecionar os dados e relacionar as tabelas.
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados.

  DATA: it_vbak_aux TYPE TABLE OF ty_vbak,
        it_vbap_aux TYPE TABLE OF ty_vbap,
        vg_tabix    TYPE          sy-tabix,
        soma        TYPE          bsad-dmbtr,
        subtrai     TYPE          bsad-dmbtr,
        altera      TYPE          bsad-dmbtr,
        cont        TYPE          n LENGTH 4,
        it_tvak     TYPE TABLE OF tvak WITH HEADER LINE,
        wa_tvak     TYPE tvak.

  RANGES: rg_auart FOR vbak-auart.
  REFRESH: it_vbak                    ,
           it_vbap                    ,
           rg_auart                   .

  IF p_auart[] IS INITIAL.

    SELECT *
    FROM setleaf
    INTO TABLE it_setleaf
     WHERE setname EQ 'ZFIS19'.

    LOOP AT it_setleaf INTO wa_setleaf.
      rg_auart-sign   = 'I'.
      rg_auart-option = 'EQ'.
      rg_auart-low    = wa_setleaf-valfrom(4).
      APPEND rg_auart.
      CLEAR: rg_auart.
    ENDLOOP.

  ENDIF.

  SELECT vbeln
         erdat
         auart
         audat
         vkorg
         vkbur
         vtweg
         spart
         kunnr
         waerk
         knumv
         vgbel
         kvgr2
    FROM vbak
    INTO TABLE it_vbak
    WHERE erdat IN p_period.


  CHECK sy-subrc IS INITIAL.

  DELETE it_vbak WHERE vkorg NOT IN p_vkorg.
  DELETE it_vbak WHERE vtweg NOT IN p_vtweg.
  DELETE it_vbak WHERE spart NOT IN p_spart.
  DELETE it_vbak WHERE kunnr NOT IN p_kunnr.
  DELETE it_vbak WHERE waerk NOT IN p_waerk.
  DELETE it_vbak WHERE auart NOT IN p_auart.
*  DELETE it_vbak WHERE vbeln NOT IN p_vbeln.
  DELETE it_vbak WHERE vkbur NOT IN p_vkbur.

  CHECK it_vbak[] IS NOT INITIAL.

  IF  p_kunnr IS INITIAL AND p_vbeln IS NOT INITIAL .
    it_vbakii[] = it_vbak[].
    DELETE it_vbakii WHERE vbeln NOT IN p_vbeln.
    SORT it_vbakii BY kunnr.
    DELETE ADJACENT DUPLICATES FROM it_vbakii COMPARING kunnr.
    p_kunnr-sign = 'I'.
    p_kunnr-option = 'EQ'.
    LOOP AT  it_vbakii INTO wa_vbak.
      MOVE wa_vbak-kunnr TO p_kunnr-low.
      APPEND p_kunnr.
      CLEAR wa_vbak.
    ENDLOOP.
    DELETE it_vbak WHERE kunnr NOT IN p_kunnr.
  ENDIF.

  IF p_auart[] IS NOT INITIAL.
    DELETE it_vbak WHERE auart NOT IN p_auart.
  ELSE.
    DELETE it_vbak WHERE auart IN rg_auart.
  ENDIF.



  IF p_auart[] IS NOT INITIAL.
    DELETE it_vbak WHERE auart NOT IN p_auart.
  ELSE.
    DELETE it_vbak WHERE auart IN rg_auart.
  ENDIF.


  CHECK it_vbak[] IS NOT INITIAL.

  SELECT vbelv
         vbtyp_n
         erdat
    FROM vbfa
    INTO TABLE it_vbfa_ini
    FOR ALL ENTRIES IN it_vbak
    WHERE vbelv   EQ it_vbak-vbeln
      AND vbtyp_n IN ('J' , 'T').

  DELETE it_vbfa_ini WHERE erdat NOT IN p_period.

  SELECT vbel2
         blart
         budat
         dmbtr
         dmbe2
         shkzg
         bukrs
         belnr
         gjahr
         augbl
  FROM   bsid
  INTO TABLE it_bsid_ac
   FOR ALL ENTRIES IN it_vbak
 WHERE bukrs   EQ it_vbak-vkorg
   AND kunnr   EQ it_vbak-kunnr
   AND gjahr   IN rg_gjahr
   AND blart   EQ 'RV'
   AND vbel2   EQ it_vbak-vbeln.

  DELETE it_bsid_ac WHERE budat NOT IN p_period.

  IF it_bsid_ac IS NOT INITIAL.
    CLEAR: it_bkpf_aux.
    SELECT *
      FROM bkpf
      INTO TABLE it_bkpf_aux
       FOR ALL ENTRIES IN it_bsid_ac
     WHERE bukrs EQ it_bsid_ac-bukrs
       AND belnr EQ it_bsid_ac-belnr
       AND gjahr EQ it_bsid_ac-gjahr.

    DELETE it_bkpf_aux WHERE stblg EQ space.

    IF it_bkpf_aux IS NOT INITIAL.

      CLEAR: wa_bkpf_aux.
      SORT:  it_bkpf_aux BY bukrs belnr.

      LOOP AT it_bsid_ac INTO wa_bsid_ac.
        vg_tabix = sy-tabix.
        READ TABLE it_bkpf_aux INTO wa_bkpf_aux WITH KEY bukrs = wa_bsid_ac-bukrs belnr = wa_bsid_ac-belnr BINARY SEARCH .
        IF sy-subrc IS INITIAL.
          CLEAR: wa_bsid_ac-belnr.
          MODIFY it_bsid_ac INDEX vg_tabix FROM wa_bsid_ac TRANSPORTING belnr.
        ENDIF.
      ENDLOOP.

      DELETE it_bsid_ac WHERE belnr EQ space.
      CLEAR: wa_bsid_ac.
    ENDIF.
  ENDIF.

  SELECT vbel2
         blart
         budat
         dmbtr
         dmbe2
         shkzg
         bukrs
         belnr
         gjahr
         augbl
   FROM  bsad
   INTO TABLE it_bsad_ac
    FOR ALL ENTRIES IN it_vbak
  WHERE bukrs   EQ it_vbak-vkorg
    AND kunnr   EQ it_vbak-kunnr
    AND vbel2   EQ it_vbak-vbeln
    AND gjahr   IN rg_gjahr.

  DELETE it_bsad_ac WHERE budat NOT IN p_period.
  DELETE it_bsad_ac WHERE blart NE 'RV'.

  IF it_bsad_ac IS NOT INITIAL.
    CLEAR: it_bkpf_aux.
    SELECT *
      FROM bkpf
      INTO TABLE it_bkpf_aux
       FOR ALL ENTRIES IN it_bsad_ac
     WHERE bukrs EQ it_bsad_ac-bukrs
       AND belnr EQ it_bsad_ac-belnr
       AND gjahr EQ it_bsad_ac-gjahr.

    DELETE it_bkpf_aux WHERE stblg EQ space.


    IF it_bkpf_aux IS NOT INITIAL.
      CLEAR: wa_bkpf_aux.
      SORT : it_bkpf_aux BY bukrs belnr.

      LOOP AT it_bsad_ac INTO wa_bsad_ac.
        vg_tabix = sy-tabix.
        READ TABLE it_bkpf_aux INTO wa_bkpf_aux WITH KEY bukrs = wa_bsad_ac-bukrs belnr = wa_bsad_ac-belnr BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CLEAR: wa_bsad_ac-belnr.
          MODIFY it_bsad_ac INDEX vg_tabix FROM wa_bsad_ac TRANSPORTING belnr.
        ENDIF.
      ENDLOOP.

      DELETE it_bsad_ac  WHERE belnr EQ space.

      CLEAR: wa_bsad_ac.
    ENDIF.
  ENDIF.

  " Valores do periodo
  SELECT vbel2
         blart
         budat
         dmbtr
         dmbe2
         shkzg
         bukrs
         belnr
         gjahr
  FROM   bsid
  INTO TABLE it_bsid_fat_p
   FOR ALL ENTRIES IN it_vbak
  WHERE bukrs  EQ  it_vbak-vkorg
   AND  kunnr   EQ it_vbak-kunnr
   AND  gjahr   IN rg_gjahr
   AND  vbel2   EQ it_vbak-vbeln.


  DELETE it_bsid_fat_p WHERE blart NE 'RV'.
  DELETE it_bsid_fat_p WHERE budat NOT IN p_erdat.

  IF it_bsid_fat_p IS NOT INITIAL.

    CLEAR: it_bkpf_aux.

    SELECT *
      FROM bkpf
      INTO TABLE it_bkpf_aux
       FOR ALL ENTRIES IN it_bsid_fat_p
     WHERE bukrs EQ it_bsid_fat_p-bukrs
       AND belnr EQ it_bsid_fat_p-belnr
       AND gjahr EQ it_bsid_fat_p-gjahr.

    DELETE it_bkpf_aux WHERE stblg EQ space.

    IF it_bkpf_aux IS NOT INITIAL.
      CLEAR: wa_bkpf_aux.
      SORT : it_bkpf_aux BY bukrs belnr.

      LOOP AT it_bsid_fat_p INTO wa_bsid_fat_p.
        vg_tabix = sy-tabix.
        READ TABLE it_bkpf_aux INTO wa_bkpf_aux WITH KEY bukrs = wa_bsid_fat_p-bukrs belnr = wa_bsid_fat_p-belnr BINARY SEARCH  .
        IF sy-subrc IS INITIAL.
          CLEAR: wa_bsid_fat_p-belnr.
          MODIFY it_bsid_fat_p INDEX vg_tabix FROM wa_bsid_fat_p TRANSPORTING belnr.
        ENDIF.
      ENDLOOP.

      DELETE it_bsid_fat_p WHERE belnr EQ space.

      CLEAR: wa_bsid_fat_p.
    ENDIF.
  ENDIF.

  SELECT vbel2
       blart
       budat
       dmbtr
       dmbe2
       shkzg
       bukrs
       belnr
       gjahr
  FROM bsad
  INTO TABLE it_bsad_fat_p
  FOR ALL ENTRIES IN it_vbak
  WHERE bukrs  EQ it_vbak-vkorg
  AND kunnr   EQ it_vbak-kunnr
  AND gjahr   IN rg_gjahr
  AND vbel2   EQ it_vbak-vbeln.

  DELETE it_bsad_fat_p WHERE blart NE 'RV'.
  DELETE it_bsad_fat_p WHERE budat NOT IN p_erdat.

  IF it_bsad_fat_p IS NOT INITIAL.

    CLEAR: it_bkpf_aux.

    SELECT *
      FROM bkpf
      INTO TABLE it_bkpf_aux
       FOR ALL ENTRIES IN it_bsad_fat_p
     WHERE bukrs EQ it_bsad_fat_p-bukrs
       AND belnr EQ it_bsad_fat_p-belnr
       AND gjahr EQ it_bsad_fat_p-gjahr.

    DELETE it_bkpf_aux WHERE stblg EQ space.


    IF it_bkpf_aux IS NOT INITIAL.
      CLEAR: wa_bkpf_aux.
      SORT : it_bkpf_aux BY bukrs belnr.

      LOOP AT it_bsad_fat_p INTO wa_bsad_fat_p.
        vg_tabix = sy-tabix.
        READ TABLE it_bkpf_aux INTO wa_bkpf_aux WITH KEY bukrs = wa_bsad_fat_p-bukrs belnr = wa_bsad_fat_p-belnr BINARY SEARCH  .
        IF sy-subrc IS INITIAL.
          CLEAR: wa_bsad_fat_p-belnr.
          MODIFY it_bsad_fat_p INDEX vg_tabix FROM wa_bsad_fat_p TRANSPORTING belnr.
        ENDIF.
      ENDLOOP.

      DELETE it_bsad_fat_p  WHERE belnr EQ space.
      CLEAR: wa_bsad_fat_p.
    ENDIF.
  ENDIF.


  SELECT vbeln
         vbelv
         vbtyp_v
    FROM vbfa
    INTO TABLE it_vbfa_cont
     FOR ALL ENTRIES IN it_vbak
   WHERE vbtyp_n EQ 'C'
     AND posnv   NE space
     AND vbeln   EQ it_vbak-vbeln.

  DELETE it_vbfa_cont WHERE vbtyp_v NE 'G'.


  SELECT vbeln
         vbelv
         vbtyp_v
    FROM vbfa
   INTO TABLE it_vbfa_ov
    FOR ALL ENTRIES IN it_vbak
  WHERE vbeln   EQ it_vbak-vgbel.

  DELETE it_vbfa_ov WHERE vbtyp_v NE 'C'.


  SELECT vbeln
         vbelv
    FROM vbfa
    INTO TABLE it_vbfa_dev
     FOR ALL ENTRIES IN it_vbak
   WHERE vbtyp_n EQ 'C'
     AND vbelv   EQ it_vbak-vgbel.

  DELETE it_vbfa_dev WHERE vbtyp_v NE 'C'.


  " Capturar o Tipo de Frete
  SELECT vk~vbeln
         vk~posnr
         vk~inco1
         vk~bstkd
    FROM vbkd AS vk
   INNER JOIN vbak AS vb ON vb~vbeln EQ vk~vbeln
    INTO TABLE it_vbkd
     FOR ALL ENTRIES IN it_vbak
   WHERE vk~vbeln EQ it_vbak-vbeln.

  "Tipo de Ordem de Venda
  SELECT auart bezei
    FROM tvakt
    INTO TABLE it_tvakt
     FOR ALL ENTRIES IN it_vbak
   WHERE auart EQ it_vbak-auart
     AND spras EQ 'PT'.

  SELECT vbeln
         posnr
         matnr
         werks
         charg
    FROM vbap
    INTO TABLE it_vbap
    FOR ALL ENTRIES IN it_vbak " IT_VBAP
    WHERE vbeln EQ it_vbak-vbeln.

  DELETE it_vbap WHERE charg NOT IN p_charg.
  DELETE it_vbap WHERE werks NOT IN p_werks.
  DELETE it_vbap WHERE matnr NOT IN p_matnr.


  it_vbak_aux[] = it_vbak[].
  SORT it_vbak_aux BY knumv.
  DELETE ADJACENT DUPLICATES FROM it_vbak_aux COMPARING knumv.

  IF it_vbak_aux IS NOT INITIAL.

    SELECT FROM v_konv FIELDS knumv , kschl , kwert , kbetr FOR ALL ENTRIES IN @it_vbak_aux WHERE knumv EQ @it_vbak_aux-knumv AND kschl IN ( 'IBRX' , 'PR00' , 'ZCIF' ) INTO TABLE @it_konv .

  ENDIF.

  IF it_vbap IS NOT INITIAL.
    SELECT vbeln
           posnr
           etenr
           wmeng
      FROM vbep
      INTO TABLE it_vbep
      FOR ALL ENTRIES IN it_vbap
      WHERE vbeln EQ it_vbap-vbeln
        AND posnr EQ it_vbap-posnr.

  ENDIF.

  it_vbak_aux[] = it_vbak[].
  SORT it_vbak_aux BY kunnr.
  DELETE ADJACENT DUPLICATES FROM it_vbak_aux COMPARING kunnr.

  IF it_vbak_aux IS NOT INITIAL.
    SELECT kunnr
           name1
      FROM kna1
      INTO TABLE it_kna1
      FOR ALL ENTRIES IN it_vbak_aux
      WHERE kunnr EQ it_vbak_aux-kunnr.
  ENDIF.

  it_vbap_aux[] = it_vbap[].

  SORT it_vbap_aux BY matnr.
  DELETE ADJACENT DUPLICATES FROM it_vbap_aux COMPARING matnr.

  IF it_vbap_aux IS NOT INITIAL.

    SELECT matnr
           maktx
      FROM makt
      INTO TABLE it_makt
      FOR ALL ENTRIES IN it_vbap_aux
      WHERE matnr EQ it_vbap_aux-matnr
        AND spras EQ sy-langu.

  ENDIF.

  it_vbap_aux[] = it_vbap[].

  SORT it_vbap_aux BY werks.
  DELETE ADJACENT DUPLICATES FROM it_vbap_aux COMPARING werks.

  IF it_vbap_aux IS NOT INITIAL.

    SELECT werks
           name1
      FROM t001w
      INTO TABLE it_t001w
      FOR ALL ENTRIES IN it_vbap_aux
      WHERE werks EQ it_vbap_aux-werks.

  ENDIF.

*-----Dados Remessa-----------------
  SELECT *
    FROM vbfa
    INTO TABLE it_vbfa_ant
    FOR ALL ENTRIES IN it_vbak
    WHERE vbelv   = it_vbak-vbeln
      AND vbtyp_n IN ('R','h','T','O')."('J', 'h','T','O'). " ('J', 'T', 'H','h')

  DELETE it_vbfa_ant WHERE erdat NOT IN p_period.

  SELECT *
    FROM vbfa
    INTO TABLE it_vbfa_atu
    FOR ALL ENTRIES IN it_vbak
    WHERE vbelv   = it_vbak-vbeln
      AND vbtyp_n IN ('R','h','T','O')."('J','h','T','O'). "('J', 'T', 'H','h')
  "AND erdat   IN p_erdat.

  DELETE it_vbfa_atu WHERE erdat NOT IN p_erdat.

  " Conforme solicitado  CSB
  SELECT *
    FROM vbfa
    INTO TABLE it_vbfa_ant_atu
    FOR ALL ENTRIES IN it_vbak
    WHERE vbelv   = it_vbak-vbeln
      AND vbtyp_n IN ('J', 'T', 'O' )
      AND erdat   LT p_erdat-low.

*----Dados FI-AR--------------------
  SELECT v~vbelv
         v~posnv
         v~vbeln
         v~posnn
         v~vbtyp_n
         v~erdat
         b~bukrs
         b~kunnr
         b~vbeln
         b~dmbtr
         b~dmbe2
         b~shkzg
         b~augdt
         b~rebzg
         b~augbl
         b~belnr
         b~gjahr
    FROM vbfa AS v
    INNER JOIN bsad AS b ON b~vbeln EQ v~vbeln
    INTO TABLE it_vbfa_ar_ant
    FOR ALL ENTRIES IN it_vbak
    WHERE v~vbelv   EQ it_vbak-vbeln
      AND v~vbtyp_n IN ('M' , 'T', 'H', 'O','N', 'P' ) "ALRS
      "AND v~erdat   IN p_period
      AND b~augdt   IN p_period
      AND b~bukrs   EQ it_vbak-vkorg
      AND b~kunnr   EQ it_vbak-kunnr
      AND b~blart   IN ('RV', 'DZ', 'DA')  . " Retirado AB

  IF it_vbfa_ar_ant IS NOT INITIAL.

    SELECT bukrs
           belnr
           rebzg
           dmbtr
           dmbe2
      FROM bsid
      INTO TABLE it_bsid_ant1
       FOR ALL ENTRIES IN it_vbfa_ar_ant
     WHERE bukrs EQ it_vbfa_ar_ant-bukrs
       AND belnr EQ it_vbfa_ar_ant-augbl
       AND gjahr EQ it_vbfa_ar_ant-gjahr.

    CLEAR: it_bkpf_aux.
    SELECT *
      FROM bkpf
      INTO TABLE it_bkpf_aux
       FOR ALL ENTRIES IN it_vbfa_ar_ant
     WHERE bukrs EQ it_vbfa_ar_ant-bukrs
       AND belnr EQ it_vbfa_ar_ant-belnr
       AND gjahr EQ it_vbfa_ar_ant-gjahr.

    DELETE it_bkpf_aux WHERE stblg EQ space. "NE space


    IF it_bkpf_aux IS NOT INITIAL.
      CLEAR: wa_bkpf_aux.
      SORT : it_bkpf_aux BY bukrs belnr.

      LOOP AT it_vbfa_ar_ant INTO wa_vbfa_ar_ant.
        vg_tabix = sy-tabix.
        READ TABLE it_bkpf_aux INTO wa_bkpf_aux WITH KEY bukrs = wa_vbfa_ar_ant-bukrs belnr = wa_vbfa_ar_ant-belnr BINARY SEARCH  .
        IF sy-subrc IS INITIAL.
          CLEAR: wa_vbfa_ar_ant-belnr.
          MODIFY it_vbfa_ar_ant INDEX vg_tabix FROM wa_vbfa_ar_ant TRANSPORTING belnr.
        ENDIF.
      ENDLOOP.
      DELETE it_vbfa_ar_ant WHERE belnr EQ space.
      CLEAR: wa_vbfa_ar_ant.
    ENDIF.
  ENDIF.

  IF it_vbfa_ar_ant[] IS NOT INITIAL. " + Kenya Braga - IR207453 - 08.11.2024

    CLEAR: it_bsad_val.
    SELECT *
      FROM bsad
      INTO TABLE it_bsad_val
       FOR ALL ENTRIES IN it_vbfa_ar_ant
      WHERE rebzg EQ it_vbfa_ar_ant-belnr
        AND bukrs EQ it_vbfa_ar_ant-bukrs
        AND kunnr EQ it_vbfa_ar_ant-kunnr.


    IF it_bsad_val IS NOT INITIAL. "alrs
*    CLEAR: WA_BSAD_VAL.
*    SORT : IT_VBFA_AR_ANT BY  BELNR,
*           IT_VBAK        BY  VBELN.
*
*    LOOP AT IT_BSAD_VAL INTO WA_BSAD_VAL  .
*
*      READ TABLE IT_VBFA_AR_ANT INTO WA_VBFA_AR_ANT WITH KEY AUGBL = WA_BSAD_VAL-BELNR BINARY SEARCH.
*      IF  SY-SUBRC IS INITIAL.
*        VG_TABIX = SY-TABIX.
*        READ TABLE IT_VBAK  INTO WA_VBAK WITH KEY VBELN = WA_VBFA_AR_ANT-VBELV.
*        IF ( ( SY-SUBRC IS INITIAL ) AND  ( WA_VBAK-VGBEL IS INITIAL )  AND  ( WA_VBFA_AR_ANT-AUGBL <>  WA_BSAD_VAL-AUGBL ) ).
*          CLEAR: WA_VBFA_AR_ANT-BELNR.
*          MODIFY IT_VBFA_AR_ANT INDEX VG_TABIX FROM WA_VBFA_AR_ANT TRANSPORTING BELNR.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
*    DELETE IT_VBFA_AR_ANT WHERE BELNR EQ SPACE.
*    CLEAR: WA_VBFA_AR_ANT,WA_VBAK .
    ENDIF.

    SELECT *
    FROM bsad
    INTO TABLE it_bsad_canc
      FOR ALL ENTRIES IN it_vbfa_ar_ant
    WHERE belnr EQ it_vbfa_ar_ant-augbl
      AND bukrs EQ it_vbfa_ar_ant-bukrs
      AND kunnr EQ it_vbfa_ar_ant-kunnr.

    IF it_bsad_canc IS NOT INITIAL. " ALRS NAO DESCONTA
*    SORT : IT_BSAD_CANC  BY  BELNR.
*
*    LOOP AT IT_VBFA_AR_ANT INTO WA_VBFA_AR_ANT.
*      VG_TABIX = SY-TABIX.
*      CONT    = 0.
*      SOMA    = 0.
*      SUBTRAI = 0.
*      ALTERA  = 0.
*
*      LOOP AT IT_BSAD_CANC INTO WA_BSAD_CANC WHERE BELNR = WA_VBFA_AR_ANT-AUGBL AND UMSKS = SPACE.
*        CONT = CONT + 1.
*        IF SY-SUBRC IS INITIAL.
*          IF WA_BSAD_CANC-SHKZG = 'S'.
*            SUBTRAI = SUBTRAI + WA_BSAD_CANC-DMBTR .
*          ELSE.
*            SOMA = SOMA + WA_BSAD_CANC-DMBTR .
*          ENDIF.
*
*          IF CONT > 1 .
*            ALTERA = SOMA - SUBTRAI.
*            WA_VBFA_AR_ANT-DMBTR = ALTERA.
*            MODIFY IT_VBFA_AR_ANT INDEX VG_TABIX FROM WA_VBFA_AR_ANT TRANSPORTING DMBTR.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*    ENDLOOP.
      CLEAR: wa_vbfa_ar_ant, wa_bsad_canc.
    ENDIF.
  ENDIF. " + Kenya Braga - IR207453 - 08.11.2024

  SELECT v~vbelv
         v~posnv
         v~vbeln
         v~posnn
         v~vbtyp_n
         v~erdat
         b~bukrs
         b~kunnr
         b~vbeln
         b~dmbtr
         b~dmbe2
         b~shkzg
         b~augdt
         b~rebzg
         b~augbl
    FROM vbfa AS v
    INNER JOIN bsid AS b ON b~vbeln EQ v~vbeln
    INTO TABLE it_vbfa_ar_atu
    FOR ALL ENTRIES IN it_vbak
    WHERE v~vbelv   EQ it_vbak-vbeln
      AND v~erdat   IN p_erdat
      AND v~vbtyp_n IN ('M' , 'T', 'H', 'O')
      AND b~bukrs   EQ it_vbak-vkorg
      AND b~kunnr   EQ it_vbak-kunnr
      AND b~blart   IN ('RV', 'DZ', 'DA') .


  SELECT gdatu
         ukurs
    FROM tcurr
    INTO TABLE it_tcurr
    WHERE fcurr EQ 'USD'
      AND tcurr EQ 'BRL'
      AND kurst IN p_kurst.

*---------------------------------------------------

  SELECT v~vbelv
         v~posnv
         v~vbeln
         v~posnn
         v~vbtyp_n
         v~erdat
         b~bukrs
         b~kunnr
         b~vbeln
         b~dmbtr
         b~dmbe2
         b~shkzg
         b~augdt
         b~augbl
         b~rebzg
         b~belnr
         b~gjahr
    FROM vbfa AS v
    INNER JOIN bsad AS b ON b~vbeln EQ v~vbeln
    INTO TABLE it_vbfa_ar_pg_atu
    FOR ALL ENTRIES IN it_vbak
    WHERE v~vbelv   EQ it_vbak-vbeln
      "AND v~erdat   IN p_erdat
      AND v~vbtyp_n IN ('M' , 'T', 'H', 'O','N', 'P' ) "alrs
      AND b~augdt   IN p_erdat
      AND b~bukrs   EQ it_vbak-vkorg
      AND b~kunnr   EQ it_vbak-kunnr
      AND b~blart   IN ('RV', 'DZ', 'DA')  . " Retirado AB

  IF it_vbfa_ar_pg_atu IS NOT INITIAL.

    SELECT bukrs
           belnr
           rebzg
           dmbtr
           dmbe2
      FROM bsid
      INTO TABLE it_bsid_atu
       FOR ALL ENTRIES IN it_vbfa_ar_pg_atu
     WHERE bukrs EQ it_vbfa_ar_pg_atu-bukrs
       AND belnr EQ it_vbfa_ar_pg_atu-augbl
       AND kunnr EQ it_vbfa_ar_pg_atu-kunnr
       AND gjahr EQ it_vbfa_ar_pg_atu-gjahr.

    " Regra

    CLEAR: it_bkpf_aux.
    SELECT *
      FROM bkpf
      INTO TABLE it_bkpf_aux
       FOR ALL ENTRIES IN it_vbfa_ar_pg_atu
     WHERE bukrs EQ it_vbfa_ar_pg_atu-bukrs
       AND belnr EQ it_vbfa_ar_pg_atu-belnr
       AND gjahr EQ it_vbfa_ar_pg_atu-gjahr.

    DELETE it_bkpf_aux WHERE stblg EQ space.
    "and stblg ne space.

    IF it_bkpf_aux IS NOT INITIAL.
      CLEAR: wa_bkpf_aux.
      SORT: it_bkpf_aux BY bukrs belnr.

      LOOP AT it_vbfa_ar_pg_atu INTO wa_vbfa_ar_pg_atu.
        vg_tabix = sy-tabix.
        READ TABLE it_bkpf_aux INTO wa_bkpf_aux WITH KEY bukrs = wa_vbfa_ar_pg_atu-bukrs belnr = wa_vbfa_ar_pg_atu-belnr BINARY SEARCH  .
        IF sy-subrc IS INITIAL.
          CLEAR: wa_vbfa_ar_pg_atu-belnr.
          MODIFY it_vbfa_ar_pg_atu INDEX vg_tabix FROM wa_vbfa_ar_pg_atu TRANSPORTING belnr.
        ENDIF.
      ENDLOOP.
      DELETE it_vbfa_ar_pg_atu WHERE belnr EQ space.
      CLEAR: wa_vbfa_ar_pg_atu.

    ENDIF.

    IF it_vbfa_ar_pg_atu[] IS NOT INITIAL. " + Kenya Braga - IR207453 - 08.11.2024

      " Regra 2
      CLEAR: it_bsad_val.
      SELECT *
        FROM bsad
        INTO TABLE it_bsad_val
         FOR ALL ENTRIES IN it_vbfa_ar_pg_atu
        WHERE rebzg EQ it_vbfa_ar_pg_atu-belnr
         AND  bukrs EQ it_vbfa_ar_pg_atu-bukrs
         AND  kunnr EQ it_vbfa_ar_pg_atu-kunnr.

      IF it_bsad_val IS NOT INITIAL. "alrs
*      CLEAR: WA_BSAD_VAL.
*      SORT: IT_VBFA_AR_PG_ATU BY BELNR,
*            IT_VBAK           BY VBELN.
*
*      LOOP AT IT_BSAD_VAL INTO WA_BSAD_VAL  .
*        READ TABLE IT_VBFA_AR_PG_ATU  INTO WA_VBFA_AR_PG_ATU WITH KEY AUGBL = WA_BSAD_VAL-BELNR BINARY SEARCH. " Adicionado Augbl
*        VG_TABIX = SY-TABIX.
*        IF SY-SUBRC IS INITIAL.
*          READ TABLE IT_VBAK  INTO WA_VBAK WITH KEY VBELN = WA_VBFA_AR_PG_ATU-VBELV BINARY SEARCH.
*          IF ( ( SY-SUBRC IS INITIAL )  AND  ( WA_VBAK-VGBEL IS INITIAL ) AND ( WA_VBFA_AR_PG_ATU-AUGBL <>  WA_BSAD_VAL-AUGBL ) ) .
*            CLEAR: WA_VBFA_AR_PG_ATU-BELNR.
*            MODIFY IT_VBFA_AR_PG_ATU INDEX VG_TABIX FROM WA_VBFA_AR_PG_ATU TRANSPORTING BELNR.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*      DELETE IT_VBFA_AR_PG_ATU WHERE BELNR EQ SPACE.
*      CLEAR: WA_VBFA_AR_PG_ATU, WA_VBAK.
      ENDIF.


      SELECT *
        FROM bsad
        INTO TABLE it_bsad_canc
          FOR ALL ENTRIES IN it_vbfa_ar_pg_atu
        WHERE belnr EQ it_vbfa_ar_pg_atu-augbl
         AND  bukrs EQ it_vbfa_ar_pg_atu-bukrs
         AND  kunnr EQ it_vbfa_ar_pg_atu-kunnr.

      IF it_bsad_canc IS NOT INITIAL. " ALRS NAO DESCONTA

*      SORT: IT_BSAD_CANC BY BELNR.
*
*      LOOP AT IT_VBFA_AR_PG_ATU INTO WA_VBFA_AR_PG_ATU.
*        VG_TABIX = SY-TABIX.
*        CONT    = 0.
*        SOMA    = 0.
*        SUBTRAI = 0.
*        ALTERA  = 0.
*        LOOP AT IT_BSAD_CANC INTO WA_BSAD_CANC WHERE BELNR = WA_VBFA_AR_PG_ATU-AUGBL AND UMSKS = SPACE.
*          IF SY-SUBRC IS INITIAL.
*            CONT = CONT + 1.
*
*            IF WA_BSAD_CANC-SHKZG = 'S'.
*              SUBTRAI = SUBTRAI + WA_BSAD_CANC-DMBTR .
*            ELSE.
*              SOMA = SOMA + WA_BSAD_CANC-DMBTR .
*            ENDIF.
*
*            IF CONT > 1 .
*              ALTERA = SOMA - SUBTRAI.
*              WA_VBFA_AR_PG_ATU-DMBTR = ALTERA.
*              MODIFY IT_VBFA_AR_PG_ATU INDEX VG_TABIX FROM WA_VBFA_AR_PG_ATU TRANSPORTING DMBTR.
*            ENDIF.
*          ENDIF.
*
*        ENDLOOP.
*      ENDLOOP.
        CLEAR: wa_vbfa_ar_pg_atu, wa_bsad_canc.
      ENDIF.
    ENDIF. " + Kenya Braga - IR207453 - 08.11.2024
  ENDIF.

  SELECT v~vbelv
         v~posnv
         v~vbeln
         v~posnn
         v~vbtyp_n
         v~erdat
         b~bukrs
         b~kunnr
         b~vbeln
         b~dmbtr
         b~dmbe2
         b~shkzg
         b~augdt
         b~augbl
         b~rebzg
         b~belnr
    FROM vbfa AS v
    INNER JOIN bsad AS b ON b~vbeln EQ v~vbeln
    INTO TABLE it_vbfa_ar_pg_ant
    FOR ALL ENTRIES IN it_vbak
    WHERE v~vbelv   EQ it_vbak-vbeln
  "      AND V~ERDAT   < P_ERDAT-LOW
      AND v~vbtyp_n IN ('M' , 'T', 'H', 'O','h', 'P' ) "alrs
      AND b~bukrs   EQ it_vbak-vkorg
      AND b~kunnr   EQ it_vbak-kunnr
      AND b~blart   IN ('RV', 'DZ', 'DA')   . " Retirado AB

  SELECT v~vbelv
         v~posnv
         v~vbeln
         v~posnn
         v~vbtyp_n
         v~erdat
         b~bukrs
         b~kunnr
         b~vbeln
         b~dmbtr
         b~dmbe2
         b~shkzg
         b~augdt
         b~augbl
         b~rebzg
         b~belnr
         b~gjahr
    FROM vbfa AS v
    INNER JOIN bsad AS b ON b~vbeln EQ v~vbeln
    INTO TABLE it_vbfa_rec
    FOR ALL ENTRIES IN it_vbak
    WHERE v~vbelv   EQ it_vbak-vbeln
      AND b~augdt   IN p_erdat
      AND b~bukrs   EQ it_vbak-vkorg
      AND b~kunnr   EQ it_vbak-kunnr
      AND b~blart   IN ('RV', 'DZ', 'DA')  . " Retirado AB

  IF it_vbfa_rec IS NOT INITIAL.


    CLEAR: it_bkpf_aux.
    SELECT *
      FROM bkpf
      INTO TABLE it_bkpf_aux
       FOR ALL ENTRIES IN it_vbfa_rec
     WHERE bukrs EQ it_vbfa_rec-bukrs
       AND belnr EQ it_vbfa_rec-belnr
       AND gjahr EQ it_vbfa_rec-gjahr.
    "AND stblg NE space.

    DELETE it_bkpf_aux WHERE stblg EQ space.


    IF it_bkpf_aux IS NOT INITIAL.
      CLEAR: wa_bkpf_aux.
      SORT: it_bkpf_aux BY bukrs belnr.

      LOOP AT it_vbfa_rec INTO wa_vbfa_rec.
        vg_tabix = sy-tabix.
        READ TABLE it_bkpf_aux INTO wa_bkpf_aux WITH KEY bukrs = wa_vbfa_rec-bukrs belnr = wa_vbfa_rec-belnr BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CLEAR: wa_vbfa_rec-belnr.
          MODIFY it_vbfa_rec INDEX vg_tabix FROM wa_vbfa_rec TRANSPORTING belnr.
        ENDIF.
      ENDLOOP.
      DELETE it_vbfa_rec WHERE belnr EQ space.
      CLEAR: wa_vbfa_rec.
    ENDIF.

    IF it_vbfa_rec[] IS NOT INITIAL. " + Kenya Braga - IR207453 - 08.11.2024
      "aki
      CLEAR: it_bsad_val.
      SELECT *
        FROM bsad
        INTO TABLE it_bsad_val
         FOR ALL ENTRIES IN it_vbfa_rec
        WHERE rebzg EQ it_vbfa_rec-belnr
         AND  bukrs EQ it_vbfa_rec-bukrs
         AND  kunnr EQ it_vbfa_rec-kunnr.

      IF it_bsad_val IS NOT INITIAL.
        CLEAR: wa_bsad_val.
        SORT: it_vbfa_rec BY belnr,
              it_vbak     BY vbeln.

        LOOP AT it_bsad_val INTO wa_bsad_val  .
          READ TABLE it_vbfa_rec  INTO wa_vbfa_rec WITH KEY augbl = wa_bsad_val-belnr BINARY SEARCH. " Adicionado AUGBL
          vg_tabix = sy-tabix.
          IF sy-subrc IS INITIAL.

            READ TABLE it_vbak  INTO wa_vbak WITH KEY vbeln = wa_vbfa_rec-vbelv BINARY SEARCH.

            IF ( ( sy-subrc IS INITIAL )  AND ( wa_vbak-vgbel IS INITIAL ) AND ( wa_vbfa_rec-augbl <>  wa_bsad_val-augbl ) ) .
              CLEAR: wa_vbfa_rec-belnr.
              MODIFY it_vbfa_rec INDEX vg_tabix FROM wa_vbfa_rec TRANSPORTING belnr.
            ENDIF.
          ENDIF.
        ENDLOOP.
        DELETE it_vbfa_rec WHERE belnr EQ space.
        CLEAR: wa_vbfa_rec, wa_vbak .
      ENDIF.
    ENDIF.  " + Kenya Braga - IR207453 - 08.11.2024

    IF it_vbfa_rec[] IS NOT INITIAL. " + Kenya Braga - IR207453 - 08.11.2024

      SELECT bukrs
             belnr
             rebzg
             dmbtr
             dmbe2
             gjahr
        FROM bsid
        INTO TABLE it_bsid_fat
         FOR ALL ENTRIES IN it_vbfa_rec
       WHERE bukrs EQ it_vbfa_rec-bukrs
         AND belnr EQ it_vbfa_rec-augbl
         AND blart IN ('RV', 'DZ', 'DA')
         AND bukrs EQ it_vbfa_rec-bukrs
         AND kunnr EQ it_vbfa_rec-kunnr
         AND gjahr EQ it_vbfa_rec-gjahr.


      IF it_bsid_fat IS NOT INITIAL.
        CLEAR: it_bkpf_aux.
        SELECT *
          FROM bkpf
          INTO TABLE it_bkpf_aux
           FOR ALL ENTRIES IN it_bsid_fat
         WHERE bukrs EQ it_bsid_fat-bukrs
           AND belnr EQ it_bsid_fat-belnr
           AND gjahr EQ it_bsid_fat-gjahr.
        "AND stblg NE space.

        DELETE it_bkpf_aux WHERE stblg EQ space.

        IF it_bkpf_aux IS NOT INITIAL.
          CLEAR: wa_bkpf_aux.
          SORT: it_bkpf_aux BY bukrs belnr.
          LOOP AT it_bsid_fat INTO wa_bsid_fat.
            vg_tabix = sy-tabix.
            READ TABLE it_bkpf_aux INTO wa_bkpf_aux WITH KEY bukrs = wa_bsid_fat-bukrs belnr = wa_bsid_fat-belnr BINARY SEARCH  .
            IF sy-subrc IS INITIAL.
              CLEAR: wa_bsid_fat-belnr.
              MODIFY it_bsid_fat INDEX vg_tabix FROM wa_bsid_fat TRANSPORTING belnr.
            ENDIF.
          ENDLOOP.
          DELETE it_bsid_fat WHERE belnr EQ space.
          CLEAR: wa_bsid_fat.
        ENDIF.
      ENDIF.
    ENDIF. " + Kenya Braga - IR207453 - 08.11.2024
  ENDIF.

  SELECT bukrs
         kunnr
         belnr
         buzei
         gjahr
         dmbtr
         dmbe2
         vbel2
    FROM bsid
    INTO TABLE it_adto
     FOR ALL ENTRIES IN it_vbak
    WHERE bukrs EQ it_vbak-vkorg
      AND kunnr EQ it_vbak-kunnr
      AND umsks NE space
      AND umskz NE 'F'
      AND vbel2 EQ it_vbak-vbeln.

*  SELECT hzuon
*         belnr
*         buzei
*         gjahr
*    FROM bseg
*    INTO TABLE it_bseg
*     FOR ALL ENTRIES IN it_adto
*    WHERE belnr EQ it_adto-belnr
*      AND buzei EQ it_adto-buzei
*      AND gjahr EQ it_adto-gjahr .

  SELECT *
    FROM zsdt0059
    INTO TABLE it_zsdt0059
    WHERE bezei EQ 'VLR ACIMA DA EXPORT'.

  SELECT *
    FROM zsdt0053
    INTO TABLE it_zsdt0053.
  SORT it_zsdt0053 BY nro_sol_ov fixacao.


ENDFORM.                    "f_seleciona_dados



*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida .


  DATA: vl_index       TYPE i,
        sld_ant        TYPE vbfa-rfmng,
        sld_atu        TYPE vbfa-rfmng,
        sld_ant_atu    TYPE vbfa-rfmng,
        sld_ant_ar     TYPE vbfa-rfmng,
        sld_atu_ar     TYPE vbfa-rfmng,
        sld_recr       TYPE vbfa-rfmng,
        sld_recd       TYPE vbfa-rfmng,
        sld_atu_ar_pg  TYPE vbfa-rfmng,
        sld_atu_ar_pg2 TYPE vbfa-rfmng,
        vlr_adto_r     TYPE vbfa-rfmng,
        vlr_adto_d     TYPE vbfa-rfmng,
        sld_ant_ar2    TYPE vbfa-rfmng,
        sld_atu_ar2    TYPE vbfa-rfmng,
        p_gdatu        TYPE tcurr-gdatu,
        sl_data        TYPE c LENGTH 10,
        dt_rem         TYPE vbfa-erdat,
        vl_aux         TYPE c LENGTH 1,
        xvlrrec        TYPE bsad-dmbtr,
        xvlrrecd       TYPE bsad-dmbe2,
        val_bsad       TYPE bsad-dmbtr,
        val_bsid       TYPE bsid-dmbtr,
        vlr_fat_acu    TYPE bsad-dmbtr,
        val_bsad_fat   TYPE bsad-dmbtr,
        val_bsid_fat   TYPE bsid-dmbtr,
        vlr_fat        TYPE bsad-dmbtr,
        val_bsad_fat_d TYPE bsad-dmbe2,
        val_bsid_fat_d TYPE bsid-dmbe2,
        vlr_fat_d      TYPE bsad-dmbe2,
        vg_sy_tabix    TYPE sytabix.


  SORT: it_vbak           BY vbeln      ,
        it_vbap           BY vbeln      ,
        it_kna1           BY kunnr      ,
        it_makt           BY matnr      ,
        it_konv           BY knumv kschl,
        it_vbep           BY vbeln posnr,
        it_vbfa_ant       BY vbelv      ,
        it_vbfa_ant_atu   BY vbelv      ,
        it_vbfa_atu       BY vbelv      ,
        it_vbfa_ar_atu    BY vbelv      ,
        it_vbfa_ar_ant    BY vbelv      ,
        it_vbfa_ar_pg_atu BY vbelv      ,
        it_vbfa_ar_pg_ant BY vbelv      ,
        it_t001w          BY werks      ,
        it_tcurr          BY gdatu      ,
        it_vbkd           BY vbeln      ,
        it_tvakt          BY auart      ,
        it_vbfa_ov        BY vbeln      ,
        it_vbfa_dev       BY vbeln      ,
        it_vbfa_ini       BY vbelv      ,
        it_vbfa_rec       BY vbelv      ,
        it_bsid_atu       BY bukrs belnr,
        it_bsid_fat       BY bukrs belnr,
        it_bsid_ant1      BY bukrs belnr,
        it_adto           BY bukrs kunnr vbel2 ,
        it_bseg           BY belnr buzei gjahr,
        it_vbfa_cont      BY vbeln,
        it_bsad_ac        BY vbel2,
        it_bsid_ac        BY vbel2,
        it_bsad_fat_p     BY vbel2,
        it_bsid_fat_p     BY vbel2.

  LOOP AT it_vbak INTO wa_vbak.

    wa_saida-erdat = wa_vbak-erdat.
    wa_saida-auart = wa_vbak-auart.
    wa_saida-audat = wa_vbak-audat.
    wa_saida-vkorg = wa_vbak-vkorg.
    wa_saida-vkbur = wa_vbak-vkbur.
    wa_saida-vtweg = wa_vbak-vtweg.
    wa_saida-spart = wa_vbak-spart.
    wa_saida-waerk = wa_vbak-waerk.
    wa_saida-kunnr = wa_vbak-kunnr.
**************************************************
    IF wa_vbak-kvgr2 = 'OV'.

      READ TABLE it_vbfa_ov INTO wa_vbfa_ov WITH KEY vbeln = wa_vbak-vgbel BINARY SEARCH.
      IF wa_vbfa_ov IS INITIAL.
        READ TABLE it_vbfa_dev INTO wa_vbfa_ov WITH KEY vbelv = wa_vbak-vgbel BINARY SEARCH.
      ENDIF.
      wa_saida-vbeln = wa_vbfa_ov-vbelv.
      wa_saida-vgbel = wa_vbak-vbeln.
    ELSE.
      wa_saida-vbeln = wa_vbak-vbeln.
    ENDIF.

    IF  ( NOT p_vbeln IS INITIAL ) AND ( wa_saida-vbeln NOT IN p_vbeln ).
      CONTINUE.
    ENDIF.

    READ TABLE  it_vbfa_cont INTO wa_vbfa_cont WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.
    wa_saida-vbelv = wa_vbfa_cont-vbelv.

    "wa_saida-vbeln

    val_bsad    = 0.
    val_bsid    = 0.
    vlr_fat_acu = 0.


*    LOOP AT it_bsad_ac INTO wa_bsad_ac WHERE vbel2 = wa_vbak-vbeln.
*      IF wa_bsad_ac-shkzg = 'H'.
*        val_bsad = val_bsad + ( wa_bsad_ac-dmbtr * -1 ).
*      ELSE.
*        val_bsad = val_bsad + wa_bsad_ac-dmbtr.
*      ENDIF.
*    ENDLOOP.

*Performance
    READ TABLE it_bsad_ac  INTO wa_bsad_ac WITH KEY vbel2 = wa_vbak-vbeln BINARY SEARCH .
    IF sy-subrc IS INITIAL.
      LOOP AT it_bsad_ac INTO wa_bsad_ac FROM sy-tabix.

        IF wa_bsad_ac-vbel2 NE wa_vbak-vbeln.
          EXIT.
        ENDIF.

        IF wa_bsad_ac-shkzg = 'H'.
          IF wa_bsad_ac-augbl IS INITIAL.
            val_bsad = val_bsad + ( wa_bsad_ac-dmbtr * -1 ). "ALRS
          ENDIF.
        ELSE.
          val_bsad = val_bsad + wa_bsad_ac-dmbtr.
        ENDIF.
      ENDLOOP.
    ENDIF.


*    LOOP AT it_bsid_ac INTO wa_bsid_ac WHERE vbel2 = wa_vbak-vbeln.
*      IF wa_bsid_ac-shkzg = 'H'.
*        val_bsid = val_bsid + ( wa_bsid_ac-dmbtr * -1 ).
*      ELSE.
*        val_bsid = val_bsid + wa_bsid_ac-dmbtr.
*      ENDIF.
*    ENDLOOP.

*Performance
    READ TABLE it_bsid_ac INTO wa_bsid_ac WITH KEY vbel2 = wa_vbak-vbeln BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      LOOP AT it_bsid_ac INTO wa_bsid_ac FROM sy-tabix.
        IF wa_bsid_ac-vbel2 NE wa_vbak-vbeln.
          EXIT.
        ENDIF.
        IF wa_bsid_ac-shkzg = 'H'.
          val_bsid = val_bsid + ( wa_bsid_ac-dmbtr * -1 ).
        ELSE.
          val_bsid = val_bsid + wa_bsid_ac-dmbtr.
        ENDIF.
      ENDLOOP.
    ENDIF.


    vlr_fat_acu = val_bsad + val_bsid.
    wa_saida-vlr_fat_acum = vlr_fat_acu.



    val_bsad_fat      = 0.
    val_bsid_fat      = 0.
    val_bsad_fat_d    = 0.
    val_bsid_fat_d    = 0.
    vlr_fat_d         = 0.


*    LOOP AT it_bsad_fat_p INTO wa_bsad_fat_p WHERE vbel2 = wa_vbak-vbeln.
*      IF wa_bsad_fat_p-shkzg = 'H'.
*        val_bsad_fat   = val_bsad_fat +   ( wa_bsad_fat_p-dmbtr * -1 ).
*        val_bsad_fat_d = val_bsad_fat_d + ( wa_bsad_fat_p-dmbe2 * -1 ).
*      ELSE.
*        val_bsad_fat   = val_bsad_fat      + wa_bsad_fat_p-dmbtr.
*        val_bsad_fat_d = val_bsad_fat_d    + wa_bsad_fat_p-dmbe2.
*      ENDIF.
*    ENDLOOP.

*Performance
    READ TABLE it_bsad_fat_p INTO wa_bsad_fat_p WITH KEY vbel2 = wa_vbak-vbeln BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      LOOP AT it_bsad_fat_p INTO wa_bsad_fat_p FROM sy-tabix.
        IF wa_bsad_fat_p-vbel2 NE wa_vbak-vbeln.
          EXIT.
        ENDIF.
        IF wa_bsad_fat_p-shkzg = 'H'.
          val_bsad_fat   = val_bsad_fat +   ( wa_bsad_fat_p-dmbtr * -1 ).
          val_bsad_fat_d = val_bsad_fat_d + ( wa_bsad_fat_p-dmbe2 * -1 ).
        ELSE.
          val_bsad_fat   = val_bsad_fat      + wa_bsad_fat_p-dmbtr.
          val_bsad_fat_d = val_bsad_fat_d    + wa_bsad_fat_p-dmbe2.
        ENDIF.
      ENDLOOP.
    ENDIF.


*    LOOP AT it_bsid_fat_p INTO wa_bsid_fat_p WHERE vbel2 = wa_vbak-vbeln.
*      IF wa_bsid_fat_p-shkzg = 'H'.
*        val_bsid_fat   = val_bsid_fat   + ( wa_bsid_fat_p-dmbtr * -1 ).
*        val_bsid_fat_d = val_bsid_fat_d + ( wa_bsid_fat_p-dmbe2 * -1 ).
*      ELSE.
*        val_bsid_fat   = val_bsid_fat   + wa_bsid_fat_p-dmbtr.
*        val_bsid_fat_d = val_bsid_fat_d + wa_bsid_fat_p-dmbe2.
*      ENDIF.
*    ENDLOOP.

*Performance
    READ TABLE it_bsid_fat_p INTO wa_bsid_fat_p WITH KEY vbel2 = wa_vbak-vbeln BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      LOOP AT it_bsid_fat_p INTO wa_bsid_fat_p  FROM sy-tabix.
        IF wa_bsid_fat_p-vbel2 NE wa_vbak-vbeln.
          EXIT.
        ENDIF.
        IF wa_bsid_fat_p-shkzg = 'H'.
          val_bsid_fat   = val_bsid_fat   + ( wa_bsid_fat_p-dmbtr * -1 ).
          val_bsid_fat_d = val_bsid_fat_d + ( wa_bsid_fat_p-dmbe2 * -1 ).
        ELSE.
          val_bsid_fat   = val_bsid_fat   + wa_bsid_fat_p-dmbtr.
          val_bsid_fat_d = val_bsid_fat_d + wa_bsid_fat_p-dmbe2.
        ENDIF.
      ENDLOOP.
    ENDIF.

    vlr_fat    = val_bsad_fat   + val_bsid_fat.
    vlr_fat_d  = val_bsad_fat_d + val_bsid_fat_d.


    " Valor Faturado Periodo
    wa_saida-vlr_fat_per_r = vlr_fat.
    wa_saida-vlr_fat_per_d = vlr_fat_d.


    " READ TABLE it_vbfa_ini INTO wa_vbfa_ini WITH KEY vbelv = wa_vbak-vbeln BINARY SEARCH.

    READ TABLE it_vbap INTO wa_vbap WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.

    IF ( ( NOT p_charg IS INITIAL ) AND ( wa_vbap-charg NOT IN p_charg ) ) OR
       ( ( NOT p_werks IS INITIAL ) AND ( wa_vbap-werks NOT IN p_werks ) ) OR
       ( ( NOT p_matnr IS INITIAL ) AND ( wa_vbap-matnr NOT IN p_matnr ) ).
      CONTINUE.
    ENDIF.

    wa_saida-werks = wa_vbap-werks.
    wa_saida-charg = wa_vbap-charg.
    wa_saida-matnr = wa_vbap-matnr.

    CLEAR sl_data.

    CONCATENATE wa_vbak-erdat+6(2) '.' wa_vbak-erdat+4(2) '.' wa_vbak-erdat(4) INTO sl_data.

    READ TABLE it_kna1 INTO wa_kna1 WITH KEY kunnr = wa_vbak-kunnr BINARY SEARCH.
    wa_saida-name1 = wa_kna1-name1 .

    READ TABLE it_makt INTO wa_makt WITH KEY matnr = wa_vbap-matnr BINARY SEARCH.
    wa_saida-maktx = wa_makt-maktx  .

    READ TABLE it_t001w INTO wa_t001w WITH KEY werks = wa_vbap-werks BINARY SEARCH.
    wa_saida-centro = wa_t001w-name1.

    "6 - Taxa de Dolar
    IF NOT sl_data = '00.00.0000'.

      CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
        EXPORTING
          input  = sl_data
        IMPORTING
          output = p_gdatu.
    ENDIF.

    READ TABLE it_tcurr INTO wa_tcurr  WITH KEY gdatu = p_gdatu BINARY SEARCH.

    IF NOT sy-subrc IS INITIAL.
      READ TABLE it_tcurr INTO wa_tcurr INDEX 1.
    ENDIF.

    READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_vbak-knumv kschl = 'IBRX' BINARY SEARCH.
    wa_saida-kwert  = wa_konv-kwert.
    "wa_saida-kbetr  = wa_konv-kbetr.
    IF sy-subrc EQ 0.
      IF wa_vbak-waerk EQ 'BRL'.
        wa_saida-tot_rs = wa_konv-kwert.
        wa_saida-tot_us = wa_konv-kwert / wa_tcurr-ukurs.

      ELSE.
        wa_saida-tot_rs = wa_konv-kwert * wa_tcurr-ukurs.
        wa_saida-tot_us = wa_konv-kwert.
      ENDIF.
    ENDIF.

    READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_vbak-knumv kschl = 'PR00' BINARY SEARCH.

    IF sy-subrc EQ 0.
      IF wa_konv-kschl EQ 'PR00'.
        IF wa_vbak-waerk EQ 'BRL'.
          wa_saida-tot_liq_rs = wa_konv-kwert.
          wa_saida-tot_liq_us = wa_konv-kwert / wa_tcurr-ukurs.
        ELSE.
          wa_saida-tot_liq_rs = wa_konv-kwert * wa_tcurr-ukurs.
          wa_saida-tot_liq_us = wa_konv-kwert.
        ENDIF.
      ENDIF.
    ENDIF.

    READ TABLE it_konv INTO wa_konv WITH KEY knumv = wa_vbak-knumv kschl = 'ZCIF' BINARY SEARCH.

    IF sy-subrc EQ 0.
      IF wa_konv-kschl EQ 'ZCIF'.
        IF wa_vbak-waerk EQ 'BRL'.
          wa_saida-tot_cif_rs = wa_konv-kwert.
          wa_saida-tot_cif_us = wa_konv-kwert / wa_tcurr-ukurs.
        ELSE.
          wa_saida-tot_cif_rs = wa_konv-kwert * wa_tcurr-ukurs.
          wa_saida-tot_cif_us = wa_konv-kwert.
        ENDIF.
      ENDIF.
    ENDIF.

    "3 - Seleção Dados de Quantidade
    READ TABLE it_vbep INTO wa_vbep WITH KEY vbeln = wa_vbap-vbeln  posnr = wa_vbap-posnr BINARY SEARCH.
    wa_saida-wmeng = wa_vbep-wmeng.

    "4.1 - Remessas entradas no período anterior
    sld_ant = 0.


*    LOOP AT it_vbfa_ant INTO wa_vbfa_ant WHERE vbelv EQ wa_vbak-vbeln .
*      IF wa_saida-vgbel IS INITIAL AND  wa_vbfa_ant-vbtyp_n EQ 'T'.
*        CONTINUE.
*      ENDIF.
*      IF ( ( wa_vbfa_ant-vbtyp_n = 'h') ).  "OR ( wa_vbfa_ant-vbtyp_n = 'H' ) ). "OR ( wa_vbfa_ant-vbtyp_n = 'T' ) ) .
*        sld_ant = sld_ant + ( wa_vbfa_ant-rfmng ) * -1.
*      ELSE.
*        IF ( ( wa_vbfa_ant-vbtyp_n = 'J') ).
*          sld_ant = sld_ant + wa_vbfa_ant-rfmng.
*        ENDIF.
*      ENDIF.
*      IF ( ( wa_vbfa_ant-vbtyp_n = 'T') AND ( wa_vbfa_ant-vbtyp_v = 'H' ) ).
*        sld_ant = sld_ant + ( wa_vbfa_ant-rfmng ).
*      ENDIF.
*    ENDLOOP.

*Performance
    READ TABLE it_vbfa_ant  INTO wa_vbfa_ant WITH KEY vbelv = wa_vbak-vbeln BINARY SEARCH .
    IF sy-subrc IS INITIAL.
      LOOP AT it_vbfa_ant INTO wa_vbfa_ant FROM sy-tabix.

        IF wa_vbfa_ant-vbelv NE wa_vbak-vbeln.
          EXIT.
        ENDIF.

        IF wa_saida-vgbel IS INITIAL AND  wa_vbfa_ant-vbtyp_n EQ 'T'.
          CONTINUE.
        ENDIF.

        IF wa_vbak-auart = 'ZREB' OR wa_vbak-auart = 'ZRRF' OR wa_vbak-auart = 'ZROF' OR wa_vbak-auart = 'ZROE' OR wa_vbak-auart = 'ZROB'.
          CONTINUE.
        ENDIF.

        IF ( ( wa_vbfa_ant-vbtyp_n = 'h') ).  "OR ( wa_vbfa_ant-vbtyp_n = 'H' ) ). "OR ( wa_vbfa_ant-vbtyp_n = 'T' ) ) .
          sld_ant = sld_ant + ( wa_vbfa_ant-rfmng ) * -1.
        ELSE.
          IF ( ( wa_vbfa_ant-vbtyp_n = 'R') )."IF ( ( WA_VBFA_ANT-VBTYP_N = 'J') ).
            sld_ant = sld_ant + wa_vbfa_ant-rfmng.
          ENDIF.
        ENDIF.
        IF ( ( wa_vbfa_ant-vbtyp_n = 'T') AND ( wa_vbfa_ant-vbtyp_v = 'H' ) ).
          sld_ant = sld_ant + ( wa_vbfa_ant-rfmng ).
        ENDIF.
      ENDLOOP.
    ENDIF.

    wa_saida-xtrant = sld_ant.

    CLEAR : dt_rem.

    "4.2 - Remessas entradas no período selecionado
    sld_atu = 0.
*    LOOP AT it_vbfa_atu INTO wa_vbfa_atu WHERE vbelv EQ wa_vbak-vbeln .
*
*      IF wa_saida-vgbel IS INITIAL AND  wa_vbfa_atu-vbtyp_n EQ 'T'.
*        CONTINUE.
*      ENDIF.
*
*      IF dt_rem IS INITIAL .
*        dt_rem  = wa_vbfa_atu-erdat.
*      ENDIF.
*
*      IF ( ( wa_vbfa_atu-vbtyp_n = 'h' ) ) . "OR ( wa_vbfa_atu-vbtyp_n = 'H' ) ). " OR ( wa_vbfa_ant-vbtyp_n = 'T' ) ).
*        sld_atu = sld_atu + ( wa_vbfa_atu-rfmng ) * -1.
*      ELSE.
*        IF ( ( wa_vbfa_atu-vbtyp_n = 'J' ) ) .
*          sld_atu = sld_atu + wa_vbfa_atu-rfmng.
*        ENDIF.
*      ENDIF.
*
*      IF ( ( wa_vbfa_atu-vbtyp_n = 'T' ) AND ( wa_vbfa_atu-vbtyp_v = 'H' ) ).
*        sld_atu = sld_atu + ( wa_vbfa_atu-rfmng ).
*      ENDIF.
*
*      CLEAR wa_vbfa_atu.
*    ENDLOOP.

*Performance
    READ TABLE it_vbfa_atu INTO wa_vbfa_atu WITH KEY vbelv = wa_vbak-vbeln BINARY SEARCH .
    IF sy-subrc IS INITIAL.
      LOOP AT it_vbfa_atu INTO wa_vbfa_atu FROM sy-tabix.

        IF wa_vbfa_atu-vbelv NE wa_vbak-vbeln.
          EXIT.
        ENDIF.

        IF wa_vbak-auart = 'ZREB' OR wa_vbak-auart = 'ZRRF' OR wa_vbak-auart = 'ZROF' OR wa_vbak-auart = 'ZROE' OR wa_vbak-auart = 'ZROB'.
          CONTINUE.
        ENDIF.

        IF wa_saida-vgbel IS INITIAL AND  wa_vbfa_atu-vbtyp_n EQ 'T'.
          CONTINUE.
        ENDIF.

        IF dt_rem IS INITIAL .
          dt_rem  = wa_vbfa_atu-erdat.
        ENDIF.

        IF ( ( wa_vbfa_atu-vbtyp_n = 'h' ) ) . "OR ( wa_vbfa_atu-vbtyp_n = 'H' ) ). " OR ( wa_vbfa_ant-vbtyp_n = 'T' ) ).
          sld_atu = sld_atu + ( wa_vbfa_atu-rfmng ) * -1.
        ELSE.
          IF ( ( wa_vbfa_atu-vbtyp_n = 'R' ) )."IF ( ( WA_VBFA_ATU-VBTYP_N = 'J' ) ) .
            sld_atu = sld_atu + wa_vbfa_atu-rfmng.
          ENDIF.
        ENDIF.

        IF ( ( wa_vbfa_atu-vbtyp_n = 'T' ) AND ( wa_vbfa_atu-vbtyp_v = 'H' ) ).
          sld_atu = sld_atu + ( wa_vbfa_atu-rfmng ).
        ENDIF.

        CLEAR wa_vbfa_atu.
      ENDLOOP.
    ENDIF.


    " CSB
    "Remessas entradas no período menor que selecionado
    sld_ant_atu = 0.
*    LOOP AT it_vbfa_ant_atu INTO wa_vbfa_ant_atu WHERE vbelv EQ wa_vbak-vbeln .
*
*      IF wa_saida-vgbel IS INITIAL AND  wa_vbfa_atu-vbtyp_n EQ 'T'.
*        CONTINUE.
*      ENDIF.
*
*      IF dt_rem IS INITIAL .
*        dt_rem  = wa_vbfa_atu-erdat.
*      ENDIF.
*
*      sld_ant_atu = sld_ant_atu + wa_vbfa_ant_atu-rfmng.
*
*      CLEAR wa_vbfa_ant_atu.
*    ENDLOOP.

*Performance
    READ TABLE it_vbfa_ant_atu INTO wa_vbfa_ant_atu WITH KEY vbelv = wa_vbak-vbeln BINARY SEARCH .
    IF sy-subrc IS INITIAL.
      LOOP AT it_vbfa_ant_atu INTO wa_vbfa_ant_atu FROM sy-tabix.

        IF wa_vbfa_ant_atu-vbelv NE wa_vbak-vbeln.
          EXIT.
        ENDIF.

        IF wa_saida-vgbel IS INITIAL AND  wa_vbfa_atu-vbtyp_n EQ 'T'.
          CONTINUE.
        ENDIF.
        IF dt_rem IS INITIAL .
          dt_rem  = wa_vbfa_atu-erdat.
        ENDIF.

        sld_ant_atu = sld_ant_atu + wa_vbfa_ant_atu-rfmng.

        CLEAR wa_vbfa_ant_atu.
      ENDLOOP.
    ENDIF.


*----- Adiantamentos
    vlr_adto_r = 0.
    vlr_adto_d = 0.


*    READ TABLE it_adto into wa_adto with key bukrs = wa_vbak-vkorg kunnr = wa_vbak-kunnr binary search.
*     Descomentado conforme chamado 46824
*    LOOP AT it_adto INTO wa_adto WHERE bukrs = wa_vbak-vkorg AND kunnr = wa_vbak-kunnr AND vbel2 = wa_vbak-vbeln .
*
*      LOOP AT it_bseg into wa_bseg where belnr = wa_adto-belnr and buzei = wa_adto-buzei and gjahr = wa_adto-gjahr .
*      READ TABLE it_bseg INTO wa_bseg WITH KEY belnr = wa_adto-belnr buzei = wa_adto-buzei gjahr = wa_adto-gjahr BINARY SEARCH.
*
*        IF wa_bseg-hzuon EQ wa_saida-vbeln.
*      vlr_adto_r = vlr_adto_r + wa_adto-dmbtr. " Real
*      vlr_adto_d = vlr_adto_d + wa_adto-dmbe2. " Dolar
*       ENDIF.
*
*    ENDLOOP.

*Performance
    READ TABLE it_adto INTO wa_adto WITH KEY bukrs = wa_vbak-vkorg  kunnr = wa_vbak-kunnr vbel2 = wa_vbak-vbeln BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      LOOP AT it_adto INTO wa_adto FROM sy-tabix.

        IF ( wa_adto-bukrs NE wa_vbak-vkorg ) OR ( wa_adto-kunnr NE wa_vbak-kunnr ) OR ( wa_adto-vbel2 NE wa_vbak-vbeln )  .
          EXIT.
        ENDIF.

        vlr_adto_r = vlr_adto_r + wa_adto-dmbtr. " Real
        vlr_adto_d = vlr_adto_d + wa_adto-dmbe2. " Dolar
      ENDLOOP.
    ENDIF.

    "READ TABLE it_bseg into wa_bseg with key belnr = wa_adto-belnr buzei = wa_adto-buzei gjahr = wa_adto-gjahr binary search.

    wa_saida-dt_rem = dt_rem.
    wa_saida-xtratu = sld_atu.
    wa_saida-xtrant_atu = sld_ant_atu.

    "wa_saida-sld_ov = wa_vbep-wmeng - wa_saida-xtrant.

    "5.3 - Faturas no período atual
    sld_atu_ar  = 0.
    sld_atu_ar2 = 0.

    sld_atu_ar_pg  = 0.
    sld_atu_ar_pg2 = 0.
*-----8.3

    xvlrrec  = 0.
    xvlrrecd = 0.

*    LOOP AT it_vbfa_ar_pg_atu INTO wa_vbfa_ar_pg_atu WHERE vbelv EQ wa_vbak-vbeln .
*
*      "IF wa_vbak-kvgr2 NE 'OV' AND wa_vbfa_ar_pg_atu-vbtyp_n = 'O'.
*      "CONTINUE.
*      "ENDIF.
*
*      READ TABLE it_bsid_atu INTO wa_bsid_aux WITH KEY bukrs = wa_vbfa_ar_pg_atu-bukrs  belnr = wa_vbfa_ar_pg_atu-augbl BINARY SEARCH.
*
*      "   IF ( ( wa_vbfa_ar_pg_atu-augdt  > wa_vbfa_ar_pg_atu-erdat ) AND ( wa_vbfa_ar_pg_atu-augdt NOT IN p_erdat ) ) OR
*      "     ( sy-subrc IS INITIAL AND wa_vbfa_ar_pg_atu-rebzg IS INITIAL ) .
*
*      IF wa_bsid_aux-rebzg = wa_vbfa_ar_pg_atu-belnr .
*        xvlrrec  = wa_vbfa_ar_pg_atu-dmbtr - wa_bsid_aux-dmbtr.
*        xvlrrecd = wa_vbfa_ar_pg_atu-dmbe2 - wa_bsid_aux-dmbe2.
*      ELSE.
*        xvlrrec  = wa_vbfa_ar_pg_atu-dmbtr.
*        xvlrrecd = wa_vbfa_ar_pg_atu-dmbe2.
*      ENDIF.
*
**        IF wa_vbfa_ar_pg_atu-shkzg = 'S' .
**          sld_atu_ar  = sld_atu_ar  + wa_vbfa_ar_pg_atu-dmbtr .
**          sld_atu_ar2 = sld_atu_ar2 + wa_vbfa_ar_pg_atu-dmbe2.
**        ELSE.
**          sld_atu_ar  = sld_atu_ar  - wa_vbfa_ar_pg_atu-dmbtr .
**          sld_atu_ar2 = sld_atu_ar2 - wa_vbfa_ar_pg_atu-dmbe2.
**        ENDIF.
**      ELSE.
*
*      IF wa_vbfa_ar_pg_atu-shkzg = 'S' .
*        sld_atu_ar_pg  = sld_atu_ar_pg  + xvlrrec ."wa_vbfa_ar_pg_atu-dmbtr .
*        sld_atu_ar_pg2 = sld_atu_ar_pg2 + xvlrrecd."wa_vbfa_ar_pg_atu-dmbe2 .
*      ELSE.
*        sld_atu_ar_pg  = sld_atu_ar_pg  - xvlrrec ."wa_vbfa_ar_pg_atu-dmbtr .
*        sld_atu_ar_pg2 = sld_atu_ar_pg2 - xvlrrecd."wa_vbfa_ar_pg_atu-dmbe2 .
*      ENDIF.
*      "   ENDIF.
*
**      IF ( ( wa_vbfa_ar_pg_atu-augdt  > wa_vbfa_ar_pg_atu-erdat ) AND ( wa_vbfa_ar_pg_atu-augdt NOT IN p_erdat ) ) OR
**        ( sy-subrc IS INITIAL AND wa_vbfa_ar_pg_atu-rebzg IS INITIAL ) .
**
**        IF wa_bsid_aux-rebzg = wa_vbfa_ar_pg_atu-belnr .
**          xvlrrec  = wa_vbfa_ar_pg_atu-dmbtr - wa_bsid_aux-dmbtr.
**          xvlrrecd = wa_vbfa_ar_pg_atu-dmbe2 - wa_bsid_aux-dmbe2.
**        else.
**          xvlrrec  = wa_vbfa_ar_pg_atu-dmbtr.
**          xvlrrecd = wa_vbfa_ar_pg_atu-dmbe2.
**        ENDIF.
**
**        IF wa_vbfa_ar_pg_atu-shkzg = 'S' .
**          sld_atu_ar  = sld_atu_ar  + wa_vbfa_ar_pg_atu-dmbtr .
**          sld_atu_ar2 = sld_atu_ar2 + wa_vbfa_ar_pg_atu-dmbe2.
**        ELSE.
**          sld_atu_ar  = sld_atu_ar  - wa_vbfa_ar_pg_atu-dmbtr .
**          sld_atu_ar2 = sld_atu_ar2 - wa_vbfa_ar_pg_atu-dmbe2.
**        ENDIF.
**      ELSE.
**
**        IF wa_vbfa_ar_pg_atu-shkzg = 'S' .
**          sld_atu_ar_pg  = sld_atu_ar_pg  + xvlrrec ."wa_vbfa_ar_pg_atu-dmbtr .
**          sld_atu_ar_pg2 = sld_atu_ar_pg2 + xvlrrecd."wa_vbfa_ar_pg_atu-dmbe2 .
**        ELSE.
**          sld_atu_ar_pg  = sld_atu_ar_pg  - xvlrrec ."wa_vbfa_ar_pg_atu-dmbtr .
**          sld_atu_ar_pg2 = sld_atu_ar_pg2 - xvlrrecd."wa_vbfa_ar_pg_atu-dmbe2 .
**        ENDIF.
**      ENDIF.
*
*      CLEAR : wa_vbfa_atu, wa_bsid_aux.
*    ENDLOOP.



*Performance
    READ TABLE it_vbfa_ar_pg_atu  INTO wa_vbfa_ar_pg_atu  WITH KEY vbelv = wa_vbak-vbeln BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      LOOP AT it_vbfa_ar_pg_atu INTO wa_vbfa_ar_pg_atu FROM sy-tabix.

        IF wa_vbfa_ar_pg_atu-vbelv NE wa_vbak-vbeln  .
          EXIT.
        ENDIF.


        READ TABLE it_bsid_atu INTO wa_bsid_aux WITH KEY bukrs = wa_vbfa_ar_pg_atu-bukrs  belnr = wa_vbfa_ar_pg_atu-augbl BINARY SEARCH.

        IF wa_bsid_aux-rebzg = wa_vbfa_ar_pg_atu-belnr .
          xvlrrec  = wa_vbfa_ar_pg_atu-dmbtr - wa_bsid_aux-dmbtr.
          xvlrrecd = wa_vbfa_ar_pg_atu-dmbe2 - wa_bsid_aux-dmbe2.
        ELSE.
          xvlrrec  = wa_vbfa_ar_pg_atu-dmbtr.
          xvlrrecd = wa_vbfa_ar_pg_atu-dmbe2.
        ENDIF.


        IF wa_vbfa_ar_pg_atu-shkzg = 'S' .
          sld_atu_ar_pg  = sld_atu_ar_pg  + xvlrrec .
          sld_atu_ar_pg2 = sld_atu_ar_pg2 + xvlrrecd.
        ELSE.
          IF wa_vbfa_ar_pg_atu-augbl IS INITIAL.
            sld_atu_ar_pg  = sld_atu_ar_pg  - xvlrrec .
            sld_atu_ar_pg2 = sld_atu_ar_pg2 - xvlrrecd.
          ENDIF.
        ENDIF.
        CLEAR : wa_vbfa_atu, wa_bsid_aux.
      ENDLOOP.
    ENDIF.

    " Valores
    wa_saida-yfrreal_2  = sld_atu_ar_pg  + vlr_adto_r.
    wa_saida-yfrdolar_2 = sld_atu_ar_pg2 + vlr_adto_d.

    "------------------------
*    LOOP AT it_vbfa_ar_atu INTO wa_vbfa_ar_atu WHERE vbelv EQ wa_vbak-vbeln .
*      "IF wa_vbak-kvgr2 NE 'OV' AND wa_vbfa_ar_atu-vbtyp_n = 'O'.
*      "CONTINUE.
*      "ENDIF.
*
*      IF wa_vbfa_ar_atu-shkzg = 'S' .
*        sld_atu_ar  = sld_atu_ar  + wa_vbfa_ar_atu-dmbtr .
*        sld_atu_ar2 = sld_atu_ar2 + wa_vbfa_ar_atu-dmbe2.
*      ELSE.
*        sld_atu_ar  = sld_atu_ar  - wa_vbfa_ar_atu-dmbtr .
*        sld_atu_ar2 = sld_atu_ar2 - wa_vbfa_ar_atu-dmbe2.
*      ENDIF.
*
*      CLEAR wa_vbfa_ant.
*    ENDLOOP.

*Performance.
    READ TABLE it_vbfa_ar_atu  INTO wa_vbfa_ar_atu  WITH KEY  vbelv = wa_vbak-vbeln BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      LOOP AT it_vbfa_ar_atu INTO wa_vbfa_ar_atu FROM sy-tabix.

        IF wa_vbfa_ar_atu-vbelv NE wa_vbak-vbeln  .
          EXIT.
        ENDIF.

        IF wa_vbfa_ar_atu-shkzg = 'S' .
          sld_atu_ar  = sld_atu_ar  + wa_vbfa_ar_atu-dmbtr .
          sld_atu_ar2 = sld_atu_ar2 + wa_vbfa_ar_atu-dmbe2.
        ELSE.
          sld_atu_ar  = sld_atu_ar  - wa_vbfa_ar_atu-dmbtr .
          sld_atu_ar2 = sld_atu_ar2 - wa_vbfa_ar_atu-dmbe2.
        ENDIF.
        CLEAR wa_vbfa_ant.
      ENDLOOP.
    ENDIF.

    wa_saida-xfrreal_2  = sld_atu_ar .
    wa_saida-xfrdolar_2 = sld_atu_ar2.

    "---------------------------------------------
    "5.2 - Faturas no período anterior
    sld_ant_ar  = 0.
    sld_ant_ar2 = 0.

    xvlrrec  = 0.
    xvlrrecd = 0.

*    LOOP AT it_vbfa_ar_ant INTO wa_vbfa_ar_ant WHERE vbelv EQ wa_vbak-vbeln .
*
*      "IF wa_vbak-kvgr2 NE 'OV' AND wa_vbfa_ar_ant-vbtyp_n = 'O'.
*      "CONTINUE.
*      "ENDIF.
*
*      READ TABLE it_bsid_ant1 INTO wa_bsid_aux WITH KEY bukrs = wa_vbfa_ar_ant-bukrs  belnr = wa_vbfa_ar_ant-augbl BINARY SEARCH.
*
*      IF wa_bsid_aux-rebzg = wa_vbfa_ar_ant-belnr .
*        xvlrrec  = wa_vbfa_ar_ant-dmbtr - wa_bsid_aux-dmbtr.
*        xvlrrecd = wa_vbfa_ar_ant-dmbe2 - wa_bsid_aux-dmbe2.
*      ELSE.
*        xvlrrec  = wa_vbfa_ar_ant-dmbtr.
*        xvlrrecd = wa_vbfa_ar_ant-dmbe2.
*      ENDIF.
*
*      IF wa_vbfa_ar_ant-shkzg = 'S' .
*        sld_ant_ar  = sld_ant_ar  + xvlrrec  ."wa_vbfa_ar_ant-dmbtr .
*        sld_ant_ar2 = sld_ant_ar2 + xvlrrecd ."wa_vbfa_ar_ant-dmbe2.
*      ELSE.
*        sld_ant_ar  = sld_ant_ar  - xvlrrec  ."wa_vbfa_ar_ant-dmbtr .
*        sld_ant_ar2 = sld_ant_ar2 - xvlrrecd ."wa_vbfa_ar_ant-dmbe2.
*      ENDIF.
*
*      CLEAR : wa_vbfa_ant, wa_bsid_aux.
*    ENDLOOP.

*Performance
    READ TABLE it_vbfa_ar_ant  INTO wa_vbfa_ar_ant  WITH KEY  vbelv = wa_vbak-vbeln BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      LOOP AT it_vbfa_ar_ant INTO wa_vbfa_ar_ant FROM sy-tabix.

        IF wa_vbfa_ar_ant-vbelv NE wa_vbak-vbeln  .
          EXIT.
        ENDIF.

        READ TABLE it_bsid_ant1 INTO wa_bsid_aux WITH KEY bukrs = wa_vbfa_ar_ant-bukrs  belnr = wa_vbfa_ar_ant-augbl BINARY SEARCH.

        IF wa_bsid_aux-rebzg = wa_vbfa_ar_ant-belnr .
          xvlrrec  = wa_vbfa_ar_ant-dmbtr - wa_bsid_aux-dmbtr.
          xvlrrecd = wa_vbfa_ar_ant-dmbe2 - wa_bsid_aux-dmbe2.
        ELSE.
          xvlrrec  = wa_vbfa_ar_ant-dmbtr.
          xvlrrecd = wa_vbfa_ar_ant-dmbe2.
        ENDIF.

        IF wa_vbfa_ar_ant-shkzg = 'S' .
          sld_ant_ar  = sld_ant_ar  + xvlrrec  ."wa_vbfa_ar_ant-dmbtr .
          sld_ant_ar2 = sld_ant_ar2 + xvlrrecd ."wa_vbfa_ar_ant-dmbe2.
        ELSE.
          IF wa_vbfa_ar_pg_atu-augbl IS INITIAL.
            sld_ant_ar  = sld_ant_ar  - xvlrrec  ."wa_vbfa_ar_ant-dmbtr .
            sld_ant_ar2 = sld_ant_ar2 - xvlrrecd ."wa_vbfa_ar_ant-dmbe2.
          ENDIF.
        ENDIF.

        CLEAR : wa_vbfa_ant, wa_bsid_aux.
      ENDLOOP.
    ENDIF.


    wa_saida-xfrreal_1  = sld_ant_ar  + vlr_adto_r.
    wa_saida-xfrdolar_1 = sld_ant_ar2 + vlr_adto_d.

*--------8.2
    sld_atu_ar  = 0.
    sld_atu_ar2 = 0.

*    LOOP AT it_vbfa_ar_pg_ant INTO wa_vbfa_ar_pg_ant WHERE vbelv EQ wa_vbak-vbeln .
*
*      "IF wa_vbak-kvgr2 NE 'OV' AND wa_vbfa_ar_pg_ant-vbtyp_n = 'O'.
*      "CONTINUE.
*      "ENDIF.
*
*      IF wa_vbfa_ar_pg_ant-shkzg = 'S' .
*        sld_atu_ar  = sld_atu_ar  + wa_vbfa_ar_pg_atu-dmbtr .
*        sld_atu_ar2 = sld_atu_ar2 + wa_vbfa_ar_pg_atu-dmbe2 .
*      ELSE.
*        sld_atu_ar  = sld_atu_ar  - wa_vbfa_ar_pg_atu-dmbtr .
*        sld_atu_ar2 = sld_atu_ar2 - wa_vbfa_ar_pg_atu-dmbe2 .
*      ENDIF.
*      CLEAR wa_vbfa_ar_pg_atu.
*    ENDLOOP.

*Performance
    READ TABLE it_vbfa_ar_pg_ant  INTO wa_vbfa_ar_pg_ant  WITH KEY  vbelv = wa_vbak-vbeln BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      LOOP AT it_vbfa_ar_pg_ant INTO wa_vbfa_ar_pg_ant FROM sy-tabix.

        IF wa_vbfa_ar_pg_ant-vbelv NE wa_vbak-vbeln  .
          EXIT.
        ENDIF.

        IF wa_vbfa_ar_pg_ant-shkzg = 'S' .
          sld_atu_ar  = sld_atu_ar  + wa_vbfa_ar_pg_atu-dmbtr .
          sld_atu_ar2 = sld_atu_ar2 + wa_vbfa_ar_pg_atu-dmbe2 .
        ELSE.
          sld_atu_ar  = sld_atu_ar  - wa_vbfa_ar_pg_atu-dmbtr .
          sld_atu_ar2 = sld_atu_ar2 - wa_vbfa_ar_pg_atu-dmbe2 .
        ENDIF.
        CLEAR wa_vbfa_ar_pg_atu.
      ENDLOOP.
    ENDIF.

    wa_saida-yfrreal_1  = sld_atu_ar.
    wa_saida-yfrdolar_1 = sld_atu_ar2.

    sld_recr = 0.
    sld_recd = 0.

*    LOOP AT it_vbfa_rec INTO wa_vbfa_rec WHERE vbelv EQ wa_vbak-vbeln .
*
*      "IF wa_vbak-kvgr2 NE 'OV' AND wa_vbfa_rec-vbtyp_n = 'O'.
*      "CONTINUE.
*      "ENDIF.
*
*      READ TABLE it_bsid_fat INTO wa_bsid_aux WITH KEY bukrs = wa_vbfa_rec-bukrs  belnr = wa_vbfa_rec-augbl BINARY SEARCH.
*
*      IF wa_bsid_aux-rebzg = wa_vbfa_rec-belnr .
*        xvlrrec  = wa_vbfa_rec-dmbtr - wa_bsid_aux-dmbtr.
*        xvlrrecd = wa_vbfa_rec-dmbe2 - wa_bsid_aux-dmbe2.
*      ELSE.
*        xvlrrec  = wa_vbfa_rec-dmbtr.
*        xvlrrecd = wa_vbfa_rec-dmbe2.
*      ENDIF.
*
*
*      "IF NOT sy-subrc IS INITIAL OR wa_vbfa_rec-rebzg IS NOT INITIAL .
*
*      IF wa_vbfa_rec-shkzg = 'S' .
*        sld_recr = sld_recr + xvlrrec ."wa_vbfa_rec-dmbtr .
*        sld_recd = sld_recd + xvlrrecd."wa_vbfa_rec-dmbe2 .
*      ELSE.
*        sld_recr = sld_recr - xvlrrec ."wa_vbfa_rec-dmbtr .
*        sld_recd = sld_recd - xvlrrecd."wa_vbfa_rec-dmbe2 .
*      ENDIF.
*
*      "ENDIF.
*
*      CLEAR : wa_vbfa_rec, wa_bsid_aux.
*    ENDLOOP.

*Performance
    READ TABLE it_vbfa_rec  INTO wa_vbfa_rec  WITH KEY  vbelv = wa_vbak-vbeln BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      LOOP AT it_vbfa_rec INTO wa_vbfa_rec FROM sy-tabix.

        IF wa_vbfa_rec-vbelv NE wa_vbak-vbeln  .
          EXIT.
        ENDIF.

        READ TABLE it_bsid_fat INTO wa_bsid_aux WITH KEY bukrs = wa_vbfa_rec-bukrs  belnr = wa_vbfa_rec-augbl BINARY SEARCH.

        IF wa_bsid_aux-rebzg = wa_vbfa_rec-belnr .
          xvlrrec  = wa_vbfa_rec-dmbtr - wa_bsid_aux-dmbtr.
          xvlrrecd = wa_vbfa_rec-dmbe2 - wa_bsid_aux-dmbe2.
        ELSE.
          xvlrrec  = wa_vbfa_rec-dmbtr.
          xvlrrecd = wa_vbfa_rec-dmbe2.
        ENDIF.

        IF wa_vbfa_rec-shkzg = 'S' .
          sld_recr = sld_recr + xvlrrec .
          sld_recd = sld_recd + xvlrrecd.
        ELSE.
          sld_recr = sld_recr - xvlrrec .
          sld_recd = sld_recd - xvlrrecd.
        ENDIF.

        CLEAR : wa_vbfa_rec, wa_bsid_aux.
      ENDLOOP.
    ENDIF.


    wa_saida-fat_rec_r = sld_recr.
    wa_saida-fat_rec_d = sld_recd.

    " Valor Faturado Periodo
    "wa_saida-vlr_fat_per_r = wa_saida-fat_rec_r + wa_saida-xfrreal_2.
    "wa_saida-vlr_fat_per_d = wa_saida-fat_rec_d + wa_saida-xfrdolar_2.


*    wa_saida-sld_rec_rs = wa_saida-tot_rs -  wa_saida-xfrreal_1 .
*    wa_saida-sld_rec_us = wa_saida-tot_us - wa_saida-xfrdolar_1 .

    IF wa_saida-wmeng > 0 .
      wa_saida-kbetr      = wa_saida-tot_rs / wa_saida-wmeng.
    ELSE.
      wa_saida-kbetr      = wa_saida-tot_rs.
    ENDIF.

    READ TABLE it_tvakt INTO wa_tvakt WITH KEY auart = wa_vbak-auart BINARY SEARCH.
    wa_saida-bezei =  wa_tvakt-bezei.

    READ TABLE it_vbkd INTO wa_vbkd WITH KEY vbeln = wa_vbak-vbeln BINARY SEARCH.

    IF NOT p_bstkd IS INITIAL AND wa_vbkd-bstkd NOT IN p_bstkd.
      CONTINUE.
    ENDIF.

    wa_saida-tipo_frete = wa_vbkd-inco1.
    wa_saida-bstkd      = wa_vbkd-bstkd.

    wa_saida-sld_ov     = ( wa_vbep-wmeng   - wa_saida-xtratu ) - wa_saida-xtrant_atu . "CSB
*    wa_saida-sld_rec_rs = ABS( wa_saida-tot_rs ) - ABS( wa_saida-yfrreal_2 ) .

    wa_saida-sld_rec_rs = abs( wa_saida-vlr_fat_acum ) - abs( wa_saida-xfrreal_1 ) . " ALRS 170912

    wa_saida-sld_rec_us = abs( wa_saida-tot_us ) - abs( wa_saida-yfrdolar_2 ) .

    vl_aux = 'N'.

    IF wa_vbak-kvgr2 = 'OV'.

*Performance
      READ TABLE it_vbfa_ini  INTO wa_vbfa_ini  WITH KEY  vbelv = wa_vbak-vbeln BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        LOOP AT it_vbfa_ini INTO wa_vbfa_ini FROM sy-tabix.

          IF wa_vbfa_ini-vbelv NE wa_vbak-vbeln  .
            EXIT.
          ENDIF.

          IF wa_vbfa_ini-vbtyp_n EQ 'T'.
            vl_aux = 'S'.
          ENDIF.
        ENDLOOP.
      ENDIF.

      IF vl_aux EQ 'S'.
        wa_saida-tot_rs     = wa_saida-tot_rs     * -1.
        wa_saida-tot_us     = wa_saida-tot_us     * -1.
        wa_saida-wmeng      = wa_saida-wmeng      * -1.
        wa_saida-xtratu     = wa_saida-xtratu     * -1.
        wa_saida-xtrant     = wa_saida-xtrant     * -1.
        wa_saida-sld_ov     = wa_saida-sld_ov     * -1.
        wa_saida-kbetr      = wa_saida-kbetr      * -1.
        wa_saida-sld_rec_rs = wa_saida-sld_rec_rs * -1.
        wa_saida-sld_rec_us = wa_saida-sld_rec_us * -1.
        wa_saida-tot_liq_rs = wa_saida-tot_liq_rs * -1.
        wa_saida-tot_liq_us = wa_saida-tot_liq_us * -1.
      ENDIF  .
    ENDIF.
*********************************************
    READ TABLE it_zsdt0053 INTO wa_zsdt0053 WITH KEY vbeln = wa_vbak-vbeln.
    IF sy-subrc IS INITIAL.
      wa_saida-nro_sol_ov = wa_zsdt0053-nro_sol_ov.
      SHIFT wa_saida-nro_sol_ov LEFT DELETING LEADING '0'. "Elimina os 0 a esquerda.
      MOVE wa_zsdt0053-navio TO wa_saida-navio." Incluido wsb
    ENDIF.

    READ TABLE it_zsdt0059 INTO wa_zsdt0059 WITH KEY nro_sol_ov = wa_zsdt0053-nro_sol_ov
                                                          posnr = wa_zsdt0053-fixacao.
    IF sy-subrc IS INITIAL.
      wa_saida-bezei = wa_zsdt0059-bezei.
      IF sy-subrc IS INITIAL.
        wa_saida-formula2 = wa_zsdt0059-formula2.
      ENDIF.
    ENDIF.
*********************************
    APPEND wa_saida TO it_saida.



    CLEAR:   wa_vbak       ,
             wa_vbap       ,
             wa_kna1       ,
             wa_makt       ,
             wa_konv       ,
             wa_vbep       ,
             wa_saida      ,
             wa_vbfa_atu   ,
             wa_vbfa_ant   ,
             wa_vbfa_ar_ant,
             wa_vbfa_ar_atu,
             wa_t001w      ,
             wa_tcurr      ,
             wa_vbkd       ,
             wa_vbfa_ov    ,
             wa_vbfa_cont  ,
             wa_bsad_ac    ,
             wa_bsid_ac    .


    CLEAR: wa_zsdt0053,wa_zsdt0059,wa_saida-formula2.
  ENDLOOP.
ENDFORM.                    " F_SAIDA

*&---------------------------------------------------------------------*
*&      Form  F_BDC_FIELD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_bdc_field  USING    VALUE(p_flag)
                           VALUE(p_fnam)
                           VALUE(p_fval).

  CLEAR t_bdc.
  IF NOT p_flag IS INITIAL.
    t_bdc-program  = p_fnam.
    t_bdc-dynpro   = p_fval.
    t_bdc-dynbegin = 'X'.
  ELSE.
    t_bdc-fnam = p_fnam.
    t_bdc-fval = p_fval.
  ENDIF.
  APPEND t_bdc.

ENDFORM.                    " F_BDC_FIELD

*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_status OUTPUT.
  SET PF-STATUS 'FF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                    "z_status OUTPUT

*&---------------------------------------------------------------------*
*&      Form  Z_LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM z_layout .
  wa_layout-zebra = 'X'.
  wa_layout-no_headers = ' '.
  wa_layout-cwidth_opt = 'X'. " Incluido wsb
  "wa_layout-grid_title = 'Flights'.
ENDFORM.                    " Z_LAYOUT

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0334   text
*      -->P_TEXT_002  text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_hot   TYPE c
                               p_zero  TYPE c.

  DATA: wl_fcat TYPE ty_estrutura."lvc_s_fcat.

  wl_fcat-tabname   = 'IT_SAIDA'.
  wl_fcat-fieldname = p_campo   .
  wl_fcat-seltext_s = p_desc    .
  wl_fcat-seltext_m = p_desc    .
  wl_fcat-seltext_l = p_desc    .
  wl_fcat-hotspot   = p_hot     .
  wl_fcat-no_zero   = p_zero    .
  wl_fcat-outputlen = p_tam     .

  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT


CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA wa_event TYPE REF TO lcl_event_receiver.

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:zm_handle_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id
                e_column_id
                es_row_no                      ,

      zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object e_interactive                   ,

      zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.


ENDCLASS.                    "lcl_event_receiver DEFINITION


*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD: zm_handle_hotspot.
    PERFORM z_handle_hotspot USING    e_row_id
                                      e_column_id
                                      es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot


  METHOD zm_handle_toolbar.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    PERFORM z_handle_command USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_user_command INPUT.
  IF sy-dynnr EQ '0100'.
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " Z_USER_COMMAND  INPUT


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM z_handle_hotspot  USING    p_e_row_id TYPE lvc_s_row
                                p_e_column_id TYPE  lvc_s_col
                                p_es_row_no TYPE  lvc_s_roid.

  CASE p_e_column_id.
    WHEN 'VBELN'.
      READ TABLE it_saida INTO wa_saida INDEX p_es_row_no-row_id.
      IF NOT wa_saida-vbeln IS INITIAL.
        SET PARAMETER ID 'AUN' FIELD wa_saida-vbeln. "preenche o campo da tela de pesquisa
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN. "chama a tela de pesquisa passando o parametro acima e ja trazendo o resultado

      ENDIF.

  ENDCASE.

ENDFORM.                    " Z_HANDLE_HOTSPOT


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*

FORM z_handle_toolbar  USING    p_object  TYPE REF TO cl_alv_event_toolbar_set
                                p_interactive TYPE char1 .

** Constants for button type
  CONSTANTS:
    c_button_normal           TYPE i VALUE 0,
    c_menu_and_default_button TYPE i VALUE 1,
    c_menu                    TYPE i VALUE 2,
    c_separator               TYPE i VALUE 3,
    c_radio_button            TYPE i VALUE 4,
    c_checkbox                TYPE i VALUE 5,
    c_menu_entry              TYPE i VALUE 6.

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.



ENDFORM.                    " Z_HANDLE_TOOLBAR


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM z_handle_command  USING p_ucomm TYPE syucomm       .



  CASE p_ucomm.
    WHEN 'REMESSA'.
*     Gera Remessa
      CALL METHOD wa_alv->refresh_table_display.
  ENDCASE.
ENDFORM.                    " Z_HANDLE_COMMAND


*&---------------------------------------------------------------------*
*&      Form  F_CONSTRUIR_CABECALHO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0510   text
*      -->P_TEXT_002  text
*----------------------------------------------------------------------*
FORM f_construir_cabecalho USING typ text.

  DATA: ls_line TYPE slis_listheader.
  ls_line-typ = typ.
  ls_line-info = text.
  APPEND ls_line TO t_top.

ENDFORM.                    " F_CONSTRUIR_CABECALHO


*&---------------------------------------------------------------------*
*&      Form  F_DEFINIR_EVENTOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_definir_eventos .
  PERFORM f_carregar_eventos USING:
                                   slis_ev_top_of_page  'XTOP_OF_PAGE'.
ENDFORM.                    " F_DEFINIR_EVENTOS

*&---------------------------------------------------------------------*
*&      Form  f_carregar_eventos
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_SLIS_EV_USER_COMMAND  text
*      -->P_0299   text
*----------------------------------------------------------------------*
FORM f_carregar_eventos USING    name form.
  CLEAR xs_events.
  xs_events-name = name.
  xs_events-form = form.
  APPEND xs_events TO events.
ENDFORM.                    " f_carregar_eventos

*---------------------------------------------------------------------*
*       FORM xtop_of_page                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM xtop_of_page.                                          "#EC CALLED

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = t_top.
*            I_LOGO             = ''.

ENDFORM. "X_TOP_PAGE


*&---------------------------------------------------------------------*
*&      Form  F_INICIAR_VARIAVES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_iniciar_variaves.

  DATA: w_texto1(10),
        w_texto2(10),
        w_texto3(40),
        periodo_r    TYPE c LENGTH 50,
        periodo_o    TYPE c LENGTH 50.


  v_report = sy-repid.

  IF p_varia IS NOT INITIAL.
    vg_variant-variant = p_varia.
  ENDIF.

*** Nome do Report

  w_texto3 = 'Resumo Geral de Ordens de Venda'.
  PERFORM f_construir_cabecalho USING 'H' w_texto3.

  CONCATENATE p_erdat-low+6(2)   '.' p_erdat-low+4(2)  '.' p_erdat-low(4)  INTO w_texto1 .
  CONCATENATE p_erdat-high+6(2)  '.' p_erdat-high+4(2) '.' p_erdat-high(4) INTO w_texto2 .

  CONCATENATE ' Periodo Remessa : ' w_texto1 ' - ' w_texto2 INTO periodo_r.

  PERFORM f_construir_cabecalho USING 'S' periodo_r.

  CONCATENATE p_period-low+6(2)   '.' p_period-low+4(2)  '.' p_period-low(4)  INTO w_texto1 .
  CONCATENATE p_period-high+6(2)  '.' p_period-high+4(2) '.' p_period-high(4) INTO w_texto2 .

  CONCATENATE ' Periodo Ordem de Venda: ' w_texto1 ' - ' w_texto2 INTO periodo_o.

  PERFORM f_construir_cabecalho USING 'S' periodo_o.

ENDFORM.                    " F_INICIAR_VARIAVES

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv.

  " PERFORM f_definir_eventos.

  PERFORM alv_preenche_cat USING:
        'NRO_SOL_OV'    TEXT-057   '11'  ' '  ' ' ,  "Nr. Solicitação
        'VBELN'         TEXT-033   '10'  'X'  'X' ,  "Nr° O.V
        'NAVIO'         TEXT-059   '20'  ' '  ' ' ,  "Navio " Incluido wsb
        'VGBEL'         TEXT-048   '20'  'X'  'X' ,
        'ERDAT'         TEXT-047   '11'  ' '  ' ' ,
        'WERKS'         TEXT-011   '8'   ' '  ' ' ,
        'VKBUR'         TEXT-056   '8'   ' '  ' ' ,
        "'CENTRO'       text-032   '30'  ' '  ' ' ,
        "'VTWEG'        text-006   '4'   ' '  ' ' ,
        'CHARG'         TEXT-012   '5'   ' '  ' ' ,
        'MATNR'         TEXT-020   '9'   ' '  'X' ,
        'MAKTX'         TEXT-010   '30'  ' '  ' ' ,
        'BSTKD'         TEXT-044   '15'  ' '  ' ' ,
        'VBELV'         TEXT-052   '10'  'X'  'X' ,
        'KUNNR'         TEXT-021   '7'   ' '  'X' ,
        'NAME1'         TEXT-008   '25'  ' '  ' ' ,
        'AUART'         TEXT-004   '7'   ' '  ' ' ,
        "'BEZEI'        text-046   '13'  ' '  ' ' ,
        "'WAERK'        text-009   '5'   ' '  ' ' ,
        "'AUDAT'        text-017   '10'  ' '  ' ' ,
        "'DT_REM'       text-045   '14'  ' '  ' ' ,
        'WMENG'         TEXT-014   '13'  ' '  ' ' ,
        'KBETR'         TEXT-022   '13'  ' '  ' ' ,
        'TOT_RS'        TEXT-023   '13'  ' '  ' ' ,
        'TOT_US'        TEXT-024   '13'  ' '  ' ' ,
        'XTRANT_ATU'    TEXT-051   '20'  ' '  ' ' , "Qt.Rem.Per.Ant
        'XTRATU'        TEXT-015   '15'  ' '  ' ' , "Qt. Rem. Período
        'XTRANT'        TEXT-016   '15'  ' '  ' ' , "Qt. Rem. Acum
        'SLD_OV'        TEXT-025   '13'  ' '  ' ' ,
        'YFRREAL_2'     TEXT-026   '19'  ' '  ' ' ,
        'YFRDOLAR_2'    TEXT-027   '19'  ' '  ' ' ,
        'XFRREAL_1'     TEXT-028   '19'  ' '  ' ' ,
        'XFRDOLAR_1'    TEXT-029   '19'  ' '  ' ' ,
        'SLD_REC_RS'    TEXT-030   '19'  ' '  ' ' ,
        'SLD_REC_US'    TEXT-031   '19'  ' '  ' ' ,
        "'XFRREAL_2'    text-035   '19'  ' '  ' ' ,
        "'XFRDOLAR_2'   text-036   '19'  ' '  ' ' ,
        'FORMULA2'      TEXT-058   '19'  ' '  ' ' ,  "Vlr. Acima Export
        "'FAT_REC_R'    text-049   '19'  ' '  ' ' ,
        "'FAT_REC_D'    text-050   '19'  ' '  ' ' ,
        "'TIPO_FRETE'   text-039   '8'   ' '  ' ' ,
        "'TOT_LIQ_RS'   text-040   '20'  ' '  ' ' ,
        "'TOT_LIQ_US'   text-041   '21'  ' '  ' ' ,
        'VLR_FAT_PER_R' TEXT-053   '22'  ' '  ' ' , " Novo
        'VLR_FAT_PER_D' TEXT-054   '22'  ' '  ' ' , " Novo
        'VLR_FAT_ACUM'  TEXT-055   '22'  ' '  ' ' . " Novo
  "'TOT_CIF_RS'   text-042   '16'  ' '  ' '  ' ',
  "'TOT_CIF_US'   text-043   '16'  ' '  ' '  ' '.


ENDFORM.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Form  F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_imprime_dados .

  IF it_saida[] IS INITIAL.
    MESSAGE i000(z01) WITH 'Não foram encontrados dados para os parametros'
                           'informados' .
    STOP.
  ENDIF.
  PERFORM f_definir_eventos.
  PERFORM f_alv_sort.
  PERFORM f_alv.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = v_report
      i_callback_user_command = 'F_USER_COMMAND'
      it_fieldcat             = it_fcat[]
      it_sort                 = t_sort[]
      i_save                  = 'X'
      it_events               = events
      is_print                = t_print
      is_variant              = vg_variant
    TABLES
      t_outtab                = it_saida.

ENDFORM.                    " F_IMPRIME_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_ALV_SORT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv_sort .

ENDFORM.                    " F_ALV_SORT

*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->L_UCOMM    text
*      -->L_SELFIELD text
*----------------------------------------------------------------------*
FORM f_user_command USING l_ucomm
                          l_selfield TYPE slis_selfield.

  CASE l_selfield-fieldname.
    WHEN 'VBELN'.
      READ TABLE it_saida INTO wa_saida INDEX l_selfield-tabindex.
      IF NOT wa_saida-vbeln IS INITIAL.
        SET PARAMETER ID 'AUN' FIELD wa_saida-vbeln. "preenche o campo da tela de pesquisa
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN. "chama a tela de pesquisa passando o parametro acima e ja trazendo o resultado

      ENDIF.
    WHEN 'VGBEL'.
      READ TABLE it_saida INTO wa_saida INDEX l_selfield-tabindex.
      IF NOT wa_saida-vgbel IS INITIAL.
        SET PARAMETER ID 'AUN' FIELD wa_saida-vgbel. "preenche o campo da tela de pesquisa
        CALL TRANSACTION 'VA03' AND SKIP FIRST SCREEN. "chama a tela de pesquisa passando o parametro acima e ja trazendo o resultado
      ENDIF.

    WHEN 'VBELV'.
      READ TABLE it_saida INTO wa_saida INDEX l_selfield-tabindex.
      IF NOT wa_saida-vbelv IS INITIAL.
        SET PARAMETER ID 'AUN' FIELD wa_saida-vbelv. "preenche o campo da tela de pesquisa
        CALL TRANSACTION 'VA43' AND SKIP FIRST SCREEN. "chama a tela de pesquisa passando o parametro acima e ja trazendo o resultado
      ENDIF.
  ENDCASE.

ENDFORM.                    "f_user_command
