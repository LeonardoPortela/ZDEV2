*&---------------------------------------------------------------------*
*& Report ZMMR0101
*& Hist:76504
*& Cham: CS2022000395 Relatório de Entrada e disponibilidade de materiais em Estoque
*& req: DEVK9A1FBD 22/03/2023
*& Transação: ZMM0211
*&---------------------------------------------------------------------*
REPORT zmmr0101.


TABLES:
  ekko,         "Cabeçalho do documento de compra
  ekpo,         "Item do documento de compras
  mseg,         "Segmento de documento - material
  mkpf.         "Cabeçalho do documento do material

TYPES:

  BEGIN OF ty_param,
    matnr     TYPE mseg-matnr,
    werks     TYPE mseg-werks,
    lgort     TYPE mseg-lgort,
    acumulado TYPE mseg-menge,
  END OF ty_param,

  BEGIN OF ty_bkpf_aux,
    mblnr TYPE mkpf-mblnr,
    mjahr TYPE mkpf-mjahr,
    awkey TYPE bkpf-awkey,
  END OF ty_bkpf_aux,

  BEGIN OF ty_ekbe,
    lfbnr TYPE ekbe-lfbnr,
    ebeln TYPE ekbe-ebeln,
    ebelp TYPE ekbe-ebelp,
    budat TYPE ekbe-budat,
  END OF ty_ekbe,

  BEGIN OF ty_resultado,
    matnr      TYPE mseg-matnr,
    werks      TYPE mseg-werks,
    lgort      TYPE mseg-lgort,
    budat      TYPE mkpf-budat,
    mblnr      TYPE mseg-mblnr,
    ebelp      TYPE mseg-ebelp,
    shkzg      TYPE mseg-shkzg,
    bukrs      TYPE mseg-bukrs,
    gjahr      TYPE mseg-gjahr,
    meins      TYPE mseg-meins,
    bwart      TYPE mseg-bwart,
    labst      TYPE p DECIMALS 2,
    ebeln      TYPE mseg-ebeln,
    lifnr      TYPE ekko-lifnr,
    name1      TYPE lfa1-name1,
    maktx      TYPE makt-maktx,
    usnam_mkpf TYPE mseg-usnam_mkpf,
    ernam      TYPE ekko-ernam,
    bednr      TYPE ekpo-bednr,
    afnam      TYPE ekpo-afnam,
    prio_urg   TYPE string,
    xblnr      TYPE mkpf-xblnr,
    lgpbe      TYPE mard-lgpbe,
    rlwrt      TYPE ekko-rlwrt,
    lfbnr      TYPE mseg-lfbnr,
    menge      TYPE p DECIMALS 2,
    dmbtr      TYPE mseg-dmbtr,
    vlr_saldo  TYPE mbew-verpr,
    verpr      TYPE mbew-verpr,
    qtddd      TYPE mseg-menge,
  END OF ty_resultado,


  BEGIN OF ty_saida,
    matnr      TYPE char10,
    werks      TYPE mseg-werks,
    lgort      TYPE mseg-lgort,
    budat_migo TYPE char10,
    mblnr      TYPE mseg-mblnr,
    ebelp      TYPE mseg-ebelp,
    shkzg      TYPE mseg-shkzg,
    bukrs      TYPE mseg-bukrs,
    gjahr      TYPE mseg-gjahr,
    meins      TYPE mseg-meins,
    bwart      TYPE mseg-bwart,
    labst      TYPE char10,
    ebeln      TYPE char10,
    lifnr      TYPE char10,
    name1      TYPE lfa1-name1,
    maktx      TYPE makt-maktx,
    ernam      TYPE ekko-ernam,
    bednr      TYPE ekpo-bednr,
    afnam      TYPE ekpo-afnam,
    prio_urg   TYPE string,
    xblnr      TYPE mkpf-xblnr,
    lgpbe      TYPE mard-lgpbe,
    rlwrt      TYPE ekko-rlwrt,
    lfbnr      TYPE mseg-lfbnr,
    menge      TYPE char10,
    dmbtr      TYPE char15,
    budat_miro TYPE char10,
    vlr_saldo  TYPE char15,
    verpr      TYPE char15,
    data_final TYPE char10,
    qtddd      TYPE char10,
    diasdisp   TYPE char10,
    usnam_mkpf TYPE mseg-usnam_mkpf,
    perfilt    TYPE string,
  END OF ty_saida.

DATA:
  it_param           TYPE TABLE OF ty_param,
  it_resultado       TYPE TABLE OF ty_resultado,
  wa_resultado       TYPE ty_resultado,
  it_acumulado       TYPE TABLE OF ty_resultado,
  wa_acumulado       TYPE ty_resultado,
  it_ekbe            TYPE TABLE OF ty_ekbe,
  wa_ekbe            TYPE ty_ekbe,
  it_bkpf_aux        TYPE TABLE OF ty_bkpf_aux,
  t_saida            TYPE TABLE OF ty_saida,
  wa_saida           TYPE ty_saida,
  t_j_1bbranch       TYPE TABLE OF j_1bbranch,
  wa_j_1bbranch      TYPE j_1bbranch,
  git_fcat_pend      TYPE          lvc_t_fcat,
  save_ok            LIKE          sy-ucomm,
  g_container        TYPE          scrfname                 VALUE 'GRID_0100_CONT1',
  g_grid             TYPE REF TO   cl_gui_alv_grid,
  g_custom_container TYPE REF TO   cl_gui_custom_container,
  gt_fieldcat        TYPE          lvc_t_fcat,
  gs_layout          TYPE          lvc_s_layo,
  g_max              TYPE          i                        VALUE 100,
  gs_spfli           TYPE          spfli,
  g_success          TYPE          c,
  lva_data(22)       TYPE          c,
  lva_status_op      TYPE          string,
  lv_awkey           TYPE bkpf-awkey,
  gob_gui_alv_grid   TYPE REF TO   cl_gui_alv_grid,
  v_empresa          TYPE string,
  v_centro           TYPE string,
  v_perido           TYPE string,
  v_perfilt          TYPE string,
  v_deposito         TYPE string,
  v_material         TYPE string,
  v_pedido           TYPE string,
  v_nf               TYPE string,
  v_fornecedor       TYPE string,
  v_requisitante     TYPE string,
  v_tpmov            TYPE string,
  pp_matnrlow        TYPE string,
  pp_matnrhigh       TYPE string,
  message            TYPE itex132.


SELECTION-SCREEN BEGIN OF BLOCK bloco1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS:
  p_bukrs FOR mseg-bukrs OBLIGATORY,                                         "Empresa
  p_werks FOR mseg-werks OBLIGATORY,                                         "Centro
  p_budat FOR mkpf-budat OBLIGATORY,                                         "Data de Entrada - Migo"
  p_lgort FOR mseg-lgort OBLIGATORY,                                         "Depósito
  p_matnr FOR mseg-matnr,                                                    "Material
  p_ebeln FOR mseg-ebeln,                                                    "Pedido
  p_xblnr FOR mkpf-xblnr,                                                    "NF
  p_lifnr FOR ekko-lifnr,                                                    "Nº Conta Fornecedor
  p_afnam FOR ekpo-afnam,                                                    "Requisitante
  p_bwart FOR mseg-bwart.                                                    "Tipo Movimento
SELECTION-SCREEN END  OF BLOCK bloco1.


START-OF-SELECTION.

  PERFORM:

  fm_selecao,
  fm_preparadados,
  fm_exibirdados.


FORM fm_selecao.


  CLEAR message.
  message = |Aguarde, selecionando dados|.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = message.


  SELECT
  mseg~matnr,
  mseg~werks,
  mseg~lgort,
  mkpf~budat,
  mseg~mblnr,
  mseg~ebelp,
  mseg~shkzg,
  mseg~bukrs,
  mseg~gjahr,
  CASE WHEN t006~isocode <> ' ' THEN t006~isocode ELSE mseg~meins END AS meins,
  mseg~bwart,
  mard~labst,
  mseg~ebeln,
  ekko~lifnr,
  lfa1~name1,
  makt~maktx,
  ekko~ernam,
  ekpo~bednr,
  ekpo~afnam,
          CASE ekpo~prio_urg
          WHEN '01'
            THEN '01 - Urgente'
          WHEN '02'
            THEN '02 - Rotina'
          ELSE '02 - Rotina'
        END AS prio_urg,
  mkpf~xblnr,
  mard~lgpbe,
 	ekko~rlwrt,
  mseg~lfbnr,
  mseg~menge,
  CASE
       WHEN ekpo~effwr > 0 THEN ekpo~effwr
     ELSE
       CASE
         WHEN mseg~dmbtr > 0 THEN mseg~dmbtr
       ELSE
       mbew~verpr * mseg~menge
       END
       END AS dmbtr,
  mbew~verpr,
  mseg~usnam_mkpf,
  mbew~verpr *  mard~labst AS vlr_saldo
  FROM mseg
    LEFT JOIN t006
         ON t006~msehi = mseg~meins
  INNER JOIN mkpf
  ON mkpf~mblnr = mseg~mblnr
  AND mkpf~mjahr = mseg~mjahr
  INNER JOIN mard
  ON mard~matnr = mseg~matnr
  AND mard~werks = mseg~werks
  AND mard~lgort = mseg~lgort
  AND mard~matnr = mseg~matnr
  INNER JOIN makt
  ON makt~matnr = mseg~matnr
  AND makt~spras = 'P'
  AND mard~labst > 0
  LEFT JOIN ekko ON ekko~ebeln = mseg~ebeln
  LEFT JOIN ekpo
  ON ekpo~ebeln = mseg~ebeln
  AND ekpo~ebelp = mseg~ebelp
  AND ekpo~matnr = mseg~matnr
  LEFT JOIN mbew ON mbew~matnr = mseg~matnr AND mbew~bwkey  = mseg~werks
  LEFT JOIN lfa1 ON lfa1~lifnr = ekko~lifnr
  WHERE 1 = 1
  AND mseg~matnr IN @p_matnr
  AND   mseg~bukrs IN @p_bukrs
  AND   mseg~werks IN @p_werks
  AND mkpf~budat IN @p_budat
  AND   mseg~shkzg = 'S'
  AND   mseg~ebeln IN @p_ebeln
  AND  mseg~lgort IN @p_lgort
  AND  mseg~bwart IN @p_bwart
  AND mkpf~xblnr IN @p_xblnr
  AND ekko~lifnr IN @p_lifnr
  AND ekpo~afnam IN @p_afnam
  AND   NOT EXISTS ( SELECT *
  FROM mseg AS mseg_e
  WHERE mseg_e~smbln = mseg~mblnr
  AND   mseg_e~sjahr = mseg~mjahr
  AND   mseg_e~smblp = mseg~zeile
  AND   mseg_e~matnr = mseg~matnr )


  INTO CORRESPONDING FIELDS OF TABLE @it_resultado.

  "Dados da MIRO.
  IF it_resultado[] IS NOT INITIAL.
    FREE: it_ekbe[].
    SELECT lfbnr, ebeln, ebelp, budat FROM ekbe
    FOR ALL ENTRIES IN @it_resultado[]
    WHERE lfbnr = @it_resultado-lfbnr
    AND ebeln = @it_resultado-ebeln AND ebelp = @it_resultado-ebelp AND vgabe = '2'
    INTO CORRESPONDING FIELDS OF TABLE @it_ekbe.

  ENDIF.



ENDFORM.


FORM fm_preparadados .

**********************************************************************

  IF p_bukrs-low IS NOT INITIAL.
    IF p_bukrs-high IS NOT INITIAL.
      v_empresa = 'Empresa: ' && p_bukrs-low && ' - ' && p_bukrs-high.
    ELSE.
      v_empresa = 'Empresa: ' && p_bukrs-low.
    ENDIF.
  ELSE.
    "v_empresa = 'Empresa: - '.
  ENDIF.

  IF p_werks-low IS NOT INITIAL.
    IF p_werks-high IS NOT INITIAL.
      v_centro = 'Centro: ' && p_werks-low && ' - ' && p_werks-high.
    ELSE.
      v_centro = 'Centro: ' && p_werks-low.
    ENDIF.
  ELSE.
    "v_centro = 'Centro: - '.
  ENDIF.


  IF p_budat-low IS NOT INITIAL.
    IF p_budat-high IS NOT INITIAL.
      v_perido =  'Período: ' &&  p_budat-low+6(2) && '.' && p_budat-low+4(2) && '.' && p_budat-low(4) && ' - ' && p_budat-high+6(2) && '.' && p_budat-high+4(2) && '.' && p_budat-high(4).
      v_perfilt = p_budat-low+6(2) && '/' && p_budat-low+4(2) && '/' && p_budat-low(4) && ' - ' && p_budat-high+6(2) && '/' && p_budat-high+4(2) && '/' && p_budat-high(4).
    ELSE.
      v_perido =  'Período: ' &&  p_budat-low+6(2) && '.' && p_budat-low+4(2) && '.' && p_budat-low(4).
      v_perfilt = p_budat-low+6(2) && '/' && p_budat-low+4(2) && '/' && p_budat-low(4).
    ENDIF.
  ELSE.
    "v_perido =  'Período: - '.
  ENDIF.

  IF p_lgort-low IS NOT INITIAL.
    IF p_lgort-high IS NOT INITIAL.
      v_deposito = 'Dépósito: ' &&  p_lgort-low && ' - ' && p_lgort-high.
    ELSE.
      v_deposito = 'Dépósito: ' &&  p_lgort-low.
    ENDIF.
  ELSE.
    "v_deposito = 'Dépósito: - '.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_matnr-low
    IMPORTING
      output = pp_matnrlow.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = p_matnr-high
    IMPORTING
      output = pp_matnrhigh.

  IF p_matnr-low IS NOT INITIAL.
    IF p_matnr-high IS NOT INITIAL.
      v_material = 'Material: ' && pp_matnrlow && ' - ' && pp_matnrhigh.
    ELSE.
      v_material = 'Material: ' && pp_matnrlow.
    ENDIF.
  ELSE.
    "v_material = 'Material: - '.
  ENDIF.

  IF p_ebeln-low IS NOT INITIAL.
    IF p_ebeln-high IS NOT INITIAL.
      v_pedido = 'Pedido: ' && p_ebeln-low && ' - ' && p_ebeln-high.
    ELSE.
      v_pedido = 'Pedido: ' && p_ebeln-low.
    ENDIF.
  ELSE.
    "v_pedido = 'Pedido: - '.
  ENDIF.


  IF p_xblnr-low IS NOT INITIAL.
    IF p_xblnr-high IS NOT INITIAL.
      v_nf = 'NF: ' && p_xblnr-low && ' - ' && p_xblnr-high.
    ELSE.
      v_nf = 'NF: ' && p_xblnr-low.
    ENDIF.
  ELSE.
    "v_nf = 'NF: - '.
  ENDIF.

  IF p_lifnr-low IS NOT INITIAL.
    IF p_lifnr-high IS NOT INITIAL.
      v_fornecedor = 'Fornecedor: ' && p_lifnr-low && ' - ' && p_lifnr-high.
    ELSE.
      v_fornecedor = 'Fornecedor: ' && p_lifnr-low.
    ENDIF.
  ELSE.
    "v_fornecedor = 'Fornecedor: - '.
  ENDIF.

  IF p_afnam-low IS NOT INITIAL.
    IF p_afnam-high IS NOT INITIAL.
      v_requisitante = 'Requisitante: ' && p_afnam-low && ' - ' && p_afnam-high.
    ELSE.
      v_requisitante = 'Requisitante: ' && p_afnam-low.
    ENDIF.
  ELSE.
    "v_requisitante = 'Requisitante: - '.
  ENDIF.


  IF p_bwart-low IS NOT INITIAL.
    IF p_bwart-high IS NOT INITIAL.
      v_tpmov = 'Tipo Movimento: ' && p_bwart-low && ' - ' && p_bwart-high.
    ELSE.
      v_tpmov = 'Tipo Movimento: ' && p_bwart-low.
    ENDIF.
  ELSE.
    "v_tpmov = 'Tipo Movimento: - '.
  ENDIF.

**********************************************************************

  CLEAR message.
  message = |Aguarde, organizandos os dados para demostrativo|.
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text = message.


  DATA: v_prox      TYPE char01,
        v_acumulado TYPE mseg-menge.

  SORT it_resultado[] BY matnr werks lgort ASCENDING budat DESCENDING.
  MOVE-CORRESPONDING it_resultado[] TO it_param[].

  DELETE ADJACENT DUPLICATES FROM it_param[] COMPARING matnr werks lgort.

  LOOP AT it_param[] INTO DATA(wa_param).
    CLEAR: wa_param-acumulado, v_prox, v_acumulado, lv_awkey.

    LOOP AT it_resultado[] INTO wa_resultado WHERE matnr EQ wa_param-matnr AND werks EQ wa_param-werks AND lgort EQ wa_param-lgort.

      ADD wa_resultado-menge TO wa_param-acumulado.

      IF wa_param-acumulado >= wa_resultado-labst.
        IF v_prox IS NOT INITIAL.
          CONTINUE.
        ELSE.
          v_prox = abap_true.
        ENDIF.
      ENDIF.

      wa_saida-werks = wa_resultado-werks.
      wa_saida-budat_migo = wa_resultado-budat+6(2) && '.' && wa_resultado-budat+4(2) && '.' && wa_resultado-budat(4).
      wa_saida-gjahr = wa_resultado-gjahr.
      wa_saida-lgort = wa_resultado-lgort.
      wa_saida-menge = wa_resultado-menge.
      wa_saida-lgpbe = wa_resultado-lgpbe.
      wa_saida-labst = wa_resultado-labst.
      wa_saida-shkzg = wa_resultado-shkzg.
      wa_saida-xblnr = wa_resultado-xblnr.
      wa_saida-usnam_mkpf = wa_resultado-usnam_mkpf.
      wa_saida-bwart = wa_resultado-bwart.


      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_resultado-matnr
        IMPORTING
          output = wa_saida-matnr.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_resultado-ebeln
        IMPORTING
          output = wa_saida-ebeln.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_resultado-ebelp
        IMPORTING
          output = wa_saida-ebelp.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_resultado-lifnr
        IMPORTING
          output = wa_saida-lifnr.
      wa_saida-bukrs = wa_resultado-bukrs.
      wa_saida-bednr = wa_resultado-bednr. "RJF
      wa_saida-afnam = wa_resultado-afnam.
      wa_saida-meins = wa_resultado-meins.
      wa_saida-name1 = wa_resultado-name1.
      wa_saida-rlwrt = wa_resultado-rlwrt.
      wa_saida-menge = wa_resultado-menge.
      wa_saida-verpr = wa_resultado-verpr.
      wa_saida-dmbtr = wa_resultado-dmbtr.
      wa_saida-maktx = wa_resultado-maktx.
      wa_saida-vlr_saldo = wa_resultado-vlr_saldo.
      lv_awkey = wa_resultado-mblnr && wa_resultado-gjahr.
      REPLACE ALL OCCURRENCES OF '.' IN wa_saida-menge WITH ','.
      REPLACE ALL OCCURRENCES OF '.' IN wa_saida-vlr_saldo WITH ','.
      REPLACE ALL OCCURRENCES OF '.' IN wa_saida-labst WITH ','.
      REPLACE ALL OCCURRENCES OF '.' IN wa_saida-dmbtr WITH ','.
      REPLACE ALL OCCURRENCES OF '.' IN wa_saida-verpr WITH ','.

      READ TABLE it_ekbe INTO DATA(wa_ekbe) WITH KEY  lfbnr = wa_resultado-lfbnr ebeln = wa_resultado-ebeln  ebelp = wa_resultado-ebelp.
      "Se tiver dados a WA_BKPF.
      IF sy-subrc EQ 0.
        "Fazer o calculo de dias entre a migo e miro.
        IF  wa_ekbe-budat IS NOT INITIAL AND  wa_resultado-budat IS NOT INITIAL.
          wa_saida-qtddd = ( wa_ekbe-budat - wa_resultado-budat ).
          REPLACE ALL OCCURRENCES OF '.00' IN wa_saida-qtddd WITH ''.
        ENDIF.
        IF wa_ekbe-budat IS NOT INITIAL.
          wa_saida-budat_miro = wa_ekbe-budat+6(2) && '.' && wa_ekbe-budat+4(2) && '.' && wa_ekbe-budat(4).
        ENDIF.
      ENDIF.

      wa_saida-diasdisp = ( p_budat-high - wa_resultado-budat ).
      REPLACE ALL OCCURRENCES OF '.00' IN wa_saida-diasdisp WITH ''.
      wa_saida-data_final = p_budat-high+6(2) && '.' && p_budat-high+4(2) && '.' && p_budat-high(4).
      wa_saida-perfilt    = v_perfilt.
      wa_saida-prio_urg   = wa_resultado-prio_urg.
      APPEND wa_saida TO t_saida.
      CLEAR: wa_saida,wa_resultado,wa_ekbe.
    ENDLOOP.

    CLEAR: wa_param, wa_saida,wa_resultado,wa_ekbe.
  ENDLOOP.
ENDFORM.


FORM fm_exibirdados .
  CALL SCREEN '0100'.
ENDFORM.

MODULE status_0100 OUTPUT.
  SET PF-STATUS 'ST0100'.
  "SET TITLEBAR 'xxx'.
  PERFORM:
  fm_alv.


ENDMODULE.


MODULE user_command_0100 INPUT.
  CASE sy-ucomm.
    WHEN 'CANCEL'.
      LEAVE PROGRAM.
    WHEN 'SAIR'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.

FORM fm_alv .


  DATA: gs_variant TYPE disvariant,
        wa_layout  TYPE lvc_s_layo.
  gs_variant-report = sy-repid.


  PERFORM fm_cria_fieldcat.

  CONCATENATE sy-datum+6(2) '.'  sy-datum+4(2) '.' sy-datum+0(4) INTO lva_data.

  IF zcl_screen=>zif_screen~set_criar_tela_padrao_report(

  EXPORTING
    i_titulo  = 'Entradas e Disponibilidade de Estoque - ' &&  lva_data
     i_filtros = VALUE zif_screen_linha_filtro_t(
   ( parametro = 'Filtros: '  valor = v_empresa       valor2 = v_centro     )
   ( parametro = ''           valor = v_perido        valor2 = v_deposito   )
   ( parametro = ''           valor = v_material      valor2 = v_pedido     )
   ( parametro = ''           valor = v_nf            valor2 = v_fornecedor )
   ( parametro = ''           valor = v_requisitante  valor2 = v_tpmov      )
   )

  CHANGING
    alv = gob_gui_alv_grid
    )
    EQ abap_true.

    wa_layout-sel_mode   = 'A'.

    CALL METHOD gob_gui_alv_grid->set_table_for_first_display
      EXPORTING
        is_variant                    = gs_variant
        i_save                        = 'A'
        is_layout                     = wa_layout
      CHANGING
        it_outtab                     = t_saida
        it_fieldcatalog               = git_fcat_pend
      EXCEPTIONS
        invalid_parameter_combination = 1
        program_error                 = 2
        too_many_lines                = 3
        OTHERS                        = 4.
  ENDIF.


ENDFORM.

FORM fm_cria_fieldcat .
  TYPES: lit_fieldcat_aux TYPE TABLE OF lvc_s_fcat WITH DEFAULT KEY.

  git_fcat_pend = VALUE lit_fieldcat_aux(
  ( tabname = 'T_SAIDA'  fieldname = 'WERKS'            coltext = 'CENTRO'                    col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'WERKS'         )
  ( tabname = 'T_SAIDA'  fieldname = 'BUDAT_MIGO'       coltext = 'DATA ENT.'                 col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'BUDAT_MIGO'    )
  ( tabname = 'T_SAIDA'  fieldname = 'LGORT'            coltext = 'DEPÓSITO'                  col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'LGORT'         )
  ( tabname = 'T_SAIDA'  fieldname = 'MATNR'            coltext = 'MATERIAL'                  col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'MATNR'         )
  ( tabname = 'T_SAIDA'  fieldname = 'MAKTX'            coltext = 'TEXTO BREVE'               col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'MAKTX'         )
  ( tabname = 'T_SAIDA'  fieldname = 'MEINS'            coltext = 'UM'                        col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'MEINS'         )
  ( tabname = 'T_SAIDA'  fieldname = 'MENGE'            coltext = 'QTD ENTRADA'               col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'MENGE'         )
  ( tabname = 'T_SAIDA'  fieldname = 'DMBTR'            coltext = 'VALOR ENTRADA'             col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'DMBTR'         )
  ( tabname = 'T_SAIDA'  fieldname = 'EBELN'            coltext = 'PEDIDO'                    col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'EBELN'         )
  ( tabname = 'T_SAIDA'  fieldname = 'EBELP'            coltext = 'ITEM PD.'                  col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'EBELP'         )
  ( tabname = 'T_SAIDA'  fieldname = 'XBLNR'            coltext = 'Nº NF'                     col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'XBLNR'         )
  ( tabname = 'T_SAIDA'  fieldname = 'LIFNR'            coltext = 'FORNECEDOR'                col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'LIFNR'         )
  ( tabname = 'T_SAIDA'  fieldname = 'NAME1'            coltext = 'NOME FORNECEDOR'           col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'NAME1'         )
  ( tabname = 'T_SAIDA'  fieldname = 'BEDNR'            coltext = 'NUM. ACOMPANHAMENTO'       col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'BEDNR'         ) "RJF
  ( tabname = 'T_SAIDA'  fieldname = 'AFNAM'            coltext = 'REQUISITANTE'              col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'AFNAM'         )
  ( tabname = 'T_SAIDA'  fieldname = 'LGPBE'            coltext = 'POSIÇÃO'                   col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'LGPBE'         )
  ( tabname = 'T_SAIDA'  fieldname = 'BWART'            coltext = 'TP. MOV.'                  col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'BWART'         )
  ( tabname = 'T_SAIDA'  fieldname = 'DATA_FINAL'       coltext = 'DATA FINAL'                col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'DATA_FINAL'    )
  ( tabname = 'T_SAIDA'  fieldname = 'LABST'            coltext = 'SALDO FINAL'               col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'LABST'         )
  ( tabname = 'T_SAIDA'  fieldname = 'VERPR'            coltext = 'VALOR UNIT'                col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'VERPR'         )
  ( tabname = 'T_SAIDA'  fieldname = 'VLR_SALDO'        coltext = 'VALOR TOTAL'               col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'VLR_SALDO'     )
  ( tabname = 'T_SAIDA'  fieldname = 'BUDAT_MIRO'       coltext = 'DATA MIRO'                 col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'BUDAT_MIRO'    )
  ( tabname = 'T_SAIDA'  fieldname = 'QTDDD'            coltext = 'MIGO X MIRO'               col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'QTDDD'         )
  ( tabname = 'T_SAIDA'  fieldname = 'USNAM_MKPF'       coltext = 'USUÁRIO'                   col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'USNAM_MKPF'    )
  ( tabname = 'T_SAIDA'  fieldname = 'DIASDISP'         coltext = 'DIAS DISPONIVEL'           col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'USNAM_MKPF'    )
  ( tabname = 'T_SAIDA'  fieldname = 'PERFILT'          coltext = 'PERIODO FILTRO'            col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'PERFILT'       )
  ( tabname = 'T_SAIDA'  fieldname = 'PRIO_URG'         coltext = 'UN'                        col_opt = 'X' no_zero = '' ref_table = 'TY_SAIDA' ref_field = 'PRIO_URG'      )
  ).

ENDFORM.
