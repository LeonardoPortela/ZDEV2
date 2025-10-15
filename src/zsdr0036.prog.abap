*&---------------------------------------------------------------------*
*& Report  ZSDT0036
*&
*&---------------------------------------------------------------------*
*&TITULO: Desbloqueio de Adiantamento
*&AUTOR : ANTONIO LUIZ RODRIGUES DA SILVA
*&DATA. : 14.10.2013
*TRANSACAO:
*&---------------------------------------------------------------------*

REPORT  zsdr0036.


*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*


DATA: BEGIN OF it_msg OCCURS 0.
        INCLUDE STRUCTURE bdcmsgcoll.
DATA: END OF it_msg.
DATA: wl_mode(1).

TYPES:
  BEGIN OF ty_sol,
    nro_sol_ov TYPE zsdt0054-nro_sol_ov,
    posnr      TYPE zsdt0054-posnr,
    vkorg      TYPE zsdt0051-vkorg,
    adiant     TYPE zsdt0054-adiant,
  END OF ty_sol,

  BEGIN OF ty_bkpf,
    bukrs TYPE bkpf-bukrs,
    belnr TYPE bkpf-belnr,
    gjahr TYPE bkpf-gjahr,
  END OF ty_bkpf,
  BEGIN OF ty_bsad,
    bukrs TYPE bsad-bukrs,
    belnr TYPE bsad-belnr,
    gjahr TYPE bsad-gjahr,
    augbl TYPE bsad-augbl,
    vbel2 TYPE bsad-vbel2,
  END OF ty_bsad.


*----------------------------------------------------------------------*
* TABELAS INTERNA
*----------------------------------------------------------------------*
DATA: it_sol  TYPE TABLE OF ty_sol,
      it_bkpf TYPE TABLE OF ty_bkpf,
      it_bsad TYPE TABLE OF ty_bsad.


*----------------------------------------------------------------------*
* WORK AREA
*----------------------------------------------------------------------*
DATA: wa_sol  TYPE ty_sol,
      wa_bkpf TYPE ty_bkpf,
      wa_bsad TYPE ty_bsad.

*----------------------------------------------------------------------*
* SHDB
*----------------------------------------------------------------------*

DATA: ti_bdcdata       TYPE STANDARD TABLE OF bdcdata ,   "Guarda o mapeamento
      wa_bdcdata       LIKE LINE OF ti_bdcdata,
      wg_documento(10).

*----------------------------------------------------------------------*
* BAPI
*----------------------------------------------------------------------*
DATA: wl_orderheaderin  TYPE bapisdh1,
      wl_orderheaderinx TYPE bapisdh1x,
      tl_return         TYPE TABLE OF bapiret2 WITH HEADER LINE,
      wl_return         TYPE bapiret2.

*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.

  PERFORM: f_seleciona_dados, " Form seleciona dados
           f_shdb. " Form de saida

END-OF-SELECTION.
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

*  SELECT ZSDT0054~NRO_SOL_OV
*         ZSDT0054~POSNR
*         ZSDT0051~VKORG
*         ZSDT0054~ADIANT
*  INTO TABLE IT_SOL
*  FROM ZSDT0054 INNER JOIN ZSDT0051
*   ON ZSDT0051~NRO_SOL_OV  = ZSDT0054~NRO_SOL_OV
*  WHERE ZSDT0054~ADIANT NE ''.

  SELECT zsdt0054~nro_sol_ov
     zsdt0054~posnr
     zsdt0051~vkorg
     zsdt0054~adiant
    INTO TABLE it_sol
    FROM zsdt0054 INNER JOIN zsdt0051
    ON zsdt0051~nro_sol_ov  = zsdt0054~nro_sol_ov
    INNER JOIN zsdt0053
    ON   zsdt0053~nro_sol_ov  = zsdt0054~nro_sol_ov
    AND  zsdt0053~posnr       = zsdt0054~posnr
    AND  zsdt0053~status      = 'B'
    WHERE zsdt0054~adiant NE ''.

  CHECK it_sol[] IS NOT INITIAL.

  SELECT bukrs belnr gjahr
    FROM bkpf
    INTO TABLE it_bkpf
    FOR ALL ENTRIES IN it_sol
    WHERE bukrs	=	it_sol-vkorg
    AND   belnr	=	it_sol-adiant
    AND   stblg = ''.

  CHECK it_bkpf[] IS NOT INITIAL.

  SELECT bukrs belnr gjahr augbl vbel2
    FROM bsad
    INTO TABLE it_bsad
    FOR ALL ENTRIES IN it_bkpf
    WHERE bukrs	=	it_bkpf-bukrs
    AND   belnr	=	it_bkpf-belnr
    AND   gjahr	=	it_bkpf-gjahr.


ENDFORM.                    " F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  F_SHDB
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_shdb .
  CHECK it_bsad[] IS NOT INITIAL.

  CLEAR: wl_orderheaderinx,wl_orderheaderin.

  SORT: it_sol BY vkorg adiant.

  DATA: wl_erro(1),
        vdata(10),
        vbstkd  TYPE vbkd-bstkd.

  CONCATENATE  sy-datum+6(2) sy-datum+4(2) sy-datum(4) INTO vdata SEPARATED BY '.'.

*  "ALRS 28/02/2014
  LOOP AT it_bsad INTO wa_bsad.
    wl_orderheaderinx-updateflag = 'U'.
    wl_orderheaderin-dlv_block   = ' '.
    wl_orderheaderinx-dlv_block   = 'X'.

    CALL FUNCTION 'BAPI_SALESORDER_CHANGE'  "#EC CI_USAGE_OK[2438131]
      EXPORTING
        salesdocument    = wa_bsad-vbel2
        order_header_in  = wl_orderheaderin
        order_header_inx = wl_orderheaderinx
      TABLES
*       ORDER_ITEM_IN    = TL_BAPISDITM
*       ORDER_ITEM_INX   = TL_BAPISDITMX
*       SCHEDULE_LINES   = TL_SCHEDULE_LINES
*       SCHEDULE_LINESX  = TL_SCHEDULE_LINESX
        return           = tl_return.

    CLEAR:wl_return.
    READ TABLE tl_return INTO wl_return WITH KEY type = 'E'.

    IF sy-subrc NE 0.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = 'X'.
      READ TABLE it_sol INTO wa_sol WITH KEY vkorg = wa_bsad-bukrs
                                         adiant = wa_bsad-belnr BINARY SEARCH.
      UPDATE zsdt0053 SET status = ''
         WHERE nro_sol_ov = wa_sol-nro_sol_ov
         AND   posnr      = wa_sol-posnr.

    ENDIF.

  ENDLOOP.

*  LOOP AT IT_BSAD INTO WA_BSAD.
*    IF WA_BSAD-VBEL2 IS INITIAL.
*      CONTINUE.
*    ENDIF.
*
*    REFRESH TI_BDCDATA.
*
*    READ TABLE IT_SOL INTO WA_SOL WITH KEY VKORG = WA_BSAD-BUKRS
*                                           ADIANT = WA_BSAD-BELNR BINARY SEARCH.
*    CONCATENATE 'SOL.OV.' WA_SOL-NRO_SOL_OV INTO VBSTKD SEPARATED BY SPACE.
*
*    PERFORM F_BDC_DATA USING:
*            'SAPMV45A'  '0102'  'X'  ''                 ' ',
*            ''          ''      ''   'BDC_OKCODE'        '/00',
*            ''          ''      ''   'VBAK-VBELN'        WA_BSAD-VBEL2,
*            'SAPMV45A'  '4001'  'X'  ''                 ' ',
*            ''          ''      ''   'BDC_OKCODE'        '=SICH',
*            ''          ''      ''   'VBAK-LIFSK'        ' '.
*
*    CLEAR WL_ERRO.
*    PERFORM ZF_CALL_TRANSACTION USING 'VA02' CHANGING WL_ERRO.
*
*    IF WL_ERRO = 'X'.
*      ROLLBACK WORK.
*    ELSE.
*      COMMIT WORK.
*      UPDATE ZSDT0053 SET STATUS = ''
*      WHERE NRO_SOL_OV = WA_SOL-NRO_SOL_OV
*      AND   POSNR      = WA_SOL-POSNR.
*    ENDIF.
*  ENDLOOP.

ENDFORM.                    " F_SHDB

*&---------------------------------------------------------------------*
*&      Form  F_BDC_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_program   programa
*      -->P_dynpro    tela
*      -->P_start     define a tela
*      -->P_fnam      nome do campo ou comando
*      -->P_fval      conteúdo do campo ou comando
*----------------------------------------------------------------------*
FORM f_bdc_data  USING p_program p_dynpro p_start p_fnam p_fval.
* Este form recebe cada conteúdo passado em ordem para os parâmetros de
* entrada e abaixo preenche a wa_bdcdata que por sua vez carrega a ti_bdcdata.
  CLEAR wa_bdcdata.
  wa_bdcdata-program   = p_program.
  wa_bdcdata-dynpro    = p_dynpro.
  wa_bdcdata-dynbegin  = p_start.
  wa_bdcdata-fnam      = p_fnam.
  wa_bdcdata-fval      = p_fval.
  APPEND wa_bdcdata TO ti_bdcdata.

ENDFORM.                    " F_BDC_DATA


*&---------------------------------------------------------------------*
*&      Form  ZF_CALL_TRANSACTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TRANS    text
*----------------------------------------------------------------------*
FORM zf_call_transaction USING p_trans CHANGING p_erro.
  CONSTANTS: c_msgid LIKE it_msg-msgid VALUE 'F5',
             c_msgnr LIKE it_msg-msgnr VALUE '312'.


  REFRESH it_msg.

  wl_mode = 'E'.

  CALL TRANSACTION p_trans USING ti_bdcdata
        MODE wl_mode
        MESSAGES INTO it_msg.

  READ TABLE it_msg WITH KEY msgtyp = 'A'.
  IF sy-subrc = 0.
    p_erro = 'X'.
  ELSE.
    READ TABLE it_msg WITH KEY msgtyp = 'E'.
    IF sy-subrc = 0.
      p_erro = 'X'.
    ENDIF.
  ENDIF.

ENDFORM.                    "ZF_CALL_TRANSACTION
