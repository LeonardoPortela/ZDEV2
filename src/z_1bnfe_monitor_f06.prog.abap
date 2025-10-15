*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_F06
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  nfe_active_read
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM nfe_active_read.

  DATA: vrefkey       TYPE j_1bnflin-refkey,
        wrbkp         TYPE rbkp,
        wbkpf         TYPE bkpf,
        vzfbdt        TYPE rbkp-zfbdt,
        vaugdt        TYPE bsak-augdt,
        wzib_nfe_forn TYPE zib_nfe_forn,
        vl_nfe_check  TYPE i.


  dt_posicao = '20101001'.

  IF contin = c_x.
    gs_contin-sign   = 'I'.
    gs_contin-option = 'EQ'.
    gs_contin-low    = c_x.
    gs_contin-high   = c_x.
    APPEND gs_contin TO gt_contin.
  ENDIF.
  IF contis = c_x.
    gs_contis-sign   = 'I'.
    gs_contis-option = 'EQ'.
    gs_contis-low    = c_x.
    gs_contis-high   = c_x.
    APPEND gs_contis TO gt_contis.
  ENDIF.
  IF cancel = c_x.
    gs_cancel-sign   = 'I'.
    gs_cancel-option = 'EQ'.
    gs_cancel-low    = c_x.
    gs_cancel-high   = c_x.
    APPEND gs_cancel TO gt_cancel.
  ENDIF.
  IF printd = c_x.
    gs_printed-sign   = 'I'.
    gs_printed-option = 'EQ'.
    gs_printed-low    = c_x.
    gs_printed-high   = c_x.
    APPEND gs_printed TO gt_printed.
  ENDIF.

  CLEAR: it_nfe_alv, it_nfe_active.

  CASE sy-tcode.
    WHEN 'ZMDFE'.
      model-sign   = 'I'.
      model-option = 'EQ'.
      model-low = '58'.
      APPEND model.
      form-sign   = 'I'.
      form-option = 'NE'.
      form-low    = space.
      APPEND form.
    WHEN 'ZNFE'.
      model-sign   = 'I'.
      model-option = 'EQ'.
      model-low = '55'.
      APPEND model.
      form-sign   = 'I'.
      form-option = 'NE'.
      form-low    = space.
      APPEND form.
    WHEN 'ZCTE'.
      model-sign   = 'I'.
      model-option = 'EQ'.
      model-low = '57'.
      APPEND model.
      form-sign   = 'I'.
      form-option = 'NE'.
      form-low    = space.
      APPEND form.
    WHEN 'ZNFE_TERC'.
      model-sign   = 'I'.
      model-option = 'EQ'.
      model-low = '55'.
      APPEND model.
      form-sign   = 'I'.
      form-option = 'EQ'.
      form-low    = space.
      APPEND form.
    WHEN 'ZCTE_TERC'.
      model-sign   = 'I'.
      model-option = 'EQ'.
      model-low = '57'.
      APPEND model.
      form-sign   = 'I'.
      form-option = 'EQ'.
      form-low    = space.
      APPEND form.
  ENDCASE.


  CLEAR: rgdoref, rgdoref[].

  IF pdocref[] IS NOT INITIAL.
    "Procurar MDF-e dos documentos referenciados
    SELECT * INTO TABLE @DATA(it_zsdt0105)
      FROM zsdt0105
     WHERE docnum IN @pdocref[].

    LOOP AT it_zsdt0105 INTO DATA(wa_zsdt0105).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_zsdt0105-docnum_ref high = wa_zsdt0105-docnum_ref ) TO rgdoref.
    ENDLOOP.
  ENDIF.

  IF pplaca[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(it_zsdt0118)
      FROM zsdt0118
     WHERE placa_cav IN @pplaca.

    LOOP AT it_zsdt0118 INTO DATA(wa_zsdt0118).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = wa_zsdt0118-docnum high = wa_zsdt0118-docnum ) TO rgdoref.
    ENDLOOP.
  ENDIF.


  "DEVK9A1VTP - Inclusão Chave de Acesso NF-e e CT-e #107011 RSA
  " Seleciona dados de Nota Fiscal.
  PERFORM seleciona_dados_nf TABLES it_nfe_active_pesq.


  IF sy-tcode EQ 'ZMDFE' AND it_nfe_active_pesq[] IS NOT INITIAL.
    SELECT * INTO TABLE @DATA(it_zsdt0102)
      FROM zsdt0102
       FOR ALL ENTRIES IN @it_nfe_active_pesq
     WHERE docnum EQ @it_nfe_active_pesq-docnum.

    SORT it_zsdt0102 BY docnum.

    SELECT * INTO TABLE @DATA(it_events)
      FROM j_1bnfe_event
       FOR ALL ENTRIES IN @it_nfe_active_pesq
     WHERE docnum    EQ @it_nfe_active_pesq-docnum
       AND int_event EQ 'EV_ENC'.

    SORT it_events BY docnum.

  ENDIF.



  LOOP AT it_nfe_active_pesq INTO wa_nfe_active.

    IF sy-tcode EQ 'ZCTE'.
      SELECT SINGLE docnum INTO wa_nfe_active-docnum
        FROM j_1bnfdoc
       WHERE docnum EQ wa_nfe_active-docnum
         "AND CANCEL EQ 'X'.
         AND nftype EQ 'F2'.
      IF sy-subrc EQ 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    APPEND wa_nfe_active TO it_nfe_active.
    MOVE-CORRESPONDING wa_nfe_active TO wa_nfe_alv.

    IF sy-tcode EQ 'ZMDFE'.
*1  A encerrar
*2  Solicitado Encerramento
*3  Encerrado
*4  Erro Encerramento
      CLEAR: wa_nfe_alv-encerrado.
      READ TABLE it_zsdt0102 INTO DATA(wa_zsdt0102) WITH KEY docnum = wa_nfe_alv-docnum BINARY SEARCH.
      IF sy-subrc IS INITIAL AND wa_nfe_alv-cancel NE abap_true AND wa_nfe_alv-action_requ CA 'C' AND wa_nfe_alv-docsta EQ '1'.
        READ TABLE it_events INTO DATA(wa_events) WITH KEY docnum = wa_nfe_alv-docnum BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          CASE wa_events-docsta.
            WHEN ' '.
              wa_nfe_alv-encerrado = icon_warning.
            WHEN '1'.
              wa_nfe_alv-encerrado = icon_complete.
            WHEN '2'.
              wa_nfe_alv-encerrado = icon_alert.
          ENDCASE.
        ELSE.
          wa_nfe_alv-encerrado = icon_transport.
        ENDIF.
      ENDIF.
    ENDIF.

    PERFORM set_status_icons CHANGING wa_nfe_alv.

    IF sy-tcode NE 'ZMDFE'.

      SELECT SINGLE * INTO @DATA(wa_zsdt0230)
        FROM zsdt0230
       WHERE docnum EQ @wa_nfe_active-docnum.

      IF sy-subrc IS INITIAL.
        wa_nfe_alv-dt_envio = wa_zsdt0230-dt_registro.
        wa_nfe_alv-hr_envio = wa_zsdt0230-hr_registro.
        wa_nfe_alv-ds_email = wa_zsdt0230-email.
      ENDIF.

    ENDIF.

    IF sy-tcode EQ 'ZCTE'.
      PERFORM set_status_ciot CHANGING wa_nfe_alv.
    ENDIF.

    IF sy-tcode EQ 'ZNFE_TERC' OR sy-tcode EQ 'ZCTE_TERC'.
      SELECT SINGLE refkey
        FROM j_1bnflin
        INTO vrefkey
        WHERE docnum EQ wa_nfe_active-docnum.

      SELECT SINGLE *
        FROM rbkp
        INTO wrbkp
        WHERE belnr EQ vrefkey.

      SELECT SINGLE *
          FROM bkpf
          INTO wbkpf
          WHERE bukrs	=	wrbkp-bukrs
          AND gjahr	=	wrbkp-gjahr
          AND awkey = vrefkey.

      SELECT SINGLE augdt
        FROM bsak
        INTO vaugdt
        WHERE bukrs	=	wbkpf-bukrs
        AND   belnr = wbkpf-belnr
        AND   gjahr = wbkpf-gjahr.

      MOVE vrefkey      TO wa_nfe_alv-refkey.
      MOVE wrbkp-usnam  TO wa_nfe_alv-usnam.
      MOVE wbkpf-belnr  TO wa_nfe_alv-belnr.
      MOVE vaugdt       TO wa_nfe_alv-augdt.
      wa_nfe_alv-zfbdt = wrbkp-zbd1t + wrbkp-zfbdt.

      SELECT SINGLE *
        FROM zib_nfe_forn
        INTO wzib_nfe_forn
        WHERE nu_chave_cnpj   =  wa_nfe_active-stcd1
      AND   nu_chave_modelo =  wa_nfe_active-model
      AND   nu_chave_serie  =  wa_nfe_active-serie
      AND   nu_chave_numero =  wa_nfe_active-nfnum9.

    ENDIF.

    "DEVK9A1VTP - 20.02.2024 SD - Inclusão Chave de Acesso NF-e e CT-e #107011 RSA
    IF sy-tcode EQ 'ZNFE' OR sy-tcode EQ 'ZMDFE'.

      CONCATENATE
              wa_nfe_alv-regio
              wa_nfe_alv-nfyear
              wa_nfe_alv-nfmonth
              wa_nfe_alv-stcd1
              wa_nfe_alv-model
              wa_nfe_alv-serie
              wa_nfe_alv-nfnum9
              wa_nfe_alv-docnum9
              wa_nfe_alv-cdv INTO wa_nfe_alv-chave.

      CLEAR vl_nfe_check.
      vl_nfe_check = strlen( wa_nfe_alv-chave ).
      IF vl_nfe_check NE 44.
        CLEAR wa_nfe_alv-chave.
      ENDIF.

    ENDIF.


*    IF sy-tcode EQ 'ZCTE'.
*
*      CONCATENATE wa_nfe_alv-regio
*                  wa_nfe_alv-nfyear
*                  wa_nfe_alv-nfmonth
*                  wa_nfe_alv-stcd1
*                  wa_nfe_alv-model
*                  wa_nfe_alv-serie
*                  wa_nfe_alv-nfnum9
*                  wa_nfe_alv-docnum9
*                  wa_nfe_alv-cdv INTO wa_nfe_alv-chavecte.
*
*    ENDIF.

    IF sy-tcode EQ 'ZCTE'.

      CLEAR wa_nfe_alv-chavecte.

*      READ TABLE t_zcte_info_nota INTO wa_zcte_info_nota WITH KEY docnum = wa_nfe_active-docnum BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        wa_nfe_alv-chavecte = wa_zcte_info_nota-chave.
*      ENDIF.
*
*      READ TABLE t_zlest0060 INTO wa_zlest0060 WITH KEY docnum = wa_nfe_active-docnum BINARY SEARCH.
*      IF sy-subrc EQ 0.
*        wa_nfe_alv-chavecte = wa_zlest0060-chave_nfe.
*      ENDIF.

        CONCATENATE wa_nfe_alv-regio
                    wa_nfe_alv-nfyear
                    wa_nfe_alv-nfmonth
                    wa_nfe_alv-stcd1
                    wa_nfe_alv-model
                    wa_nfe_alv-serie
                    wa_nfe_alv-nfnum9
                    wa_nfe_alv-docnum9
                    wa_nfe_alv-cdv INTO wa_nfe_alv-chavecte.

      CLEAR vl_nfe_check.
      vl_nfe_check = strlen( wa_nfe_alv-chavecte ).
      IF vl_nfe_check NE 44.
        CLEAR wa_nfe_alv-chavecte.
      ENDIF.

    ENDIF.


    IF st_nota IS NOT INITIAL.
      IF wzib_nfe_forn-st_nota IN st_nota.
        APPEND wa_nfe_alv TO it_nfe_alv.
      ENDIF.
    ELSE.
      APPEND wa_nfe_alv TO it_nfe_alv.
    ENDIF.

    gf_col_num_total = gf_col_num_total + c_1.

    CLEAR wzib_nfe_forn.

  ENDLOOP.

*  SELECT *
*     FROM J_1BNFE_ACTIVE
*     INTO WA_NFE_ACTIVE
*    WHERE DOCNUM      IN DOCNUM
*      AND DOCSTA      IN DOCSTA
*      AND SCSSTA      IN SCSSTA
*      AND CODE        IN CODE                               "1160312
*      AND DIRECT      IN DIRECT
*      AND CONTING     IN GT_CONTIN
*      AND CANCEL      IN GT_CANCEL
*      AND PRINTD      IN GT_PRINTED
*      AND CREDAT      IN DATE0
*      AND ACTION_DATE IN DATE
*      AND STCD1       IN STCD1
*      AND MODEL       IN MODEL
*      AND NFNUM9      IN NFNUM9
*      AND FORM        IN FORM
*      AND SERIE       IN SERIE
*      AND NFYEAR      IN NFYEAR
*      AND NFMONTH     IN NFMONT
*      AND BUKRS       IN BUKRS
*      AND BRANCH      IN BUPLA
*      AND VSTEL       IN SHIPT
*      AND ACTION_USER IN USER
*      AND CRENAM      IN USERCRE                            "1165360
*      AND CRENAM      NE 'INTERFACE'
*      AND PARID       IN PARTNER
*      AND ACTION_REQU IN USEACT
*      AND CONTING_S   IN GT_CONTIS
*      AND CREDAT      GE DT_POSICAO.
*
*    IF SY-TCODE EQ 'ZMDFE'.
*
*    ENDIF.
*
*    IF SY-TCODE EQ 'ZCTE'.
*      SELECT SINGLE DOCNUM INTO WA_NFE_ACTIVE-DOCNUM
*        FROM J_1BNFDOC
*       WHERE DOCNUM EQ WA_NFE_ACTIVE-DOCNUM
*         "AND CANCEL EQ 'X'.
*         AND NFTYPE EQ 'F2'.
*      IF SY-SUBRC EQ 0.
*        CONTINUE.
*      ENDIF.
*    ENDIF.
*
*    APPEND WA_NFE_ACTIVE TO IT_NFE_ACTIVE.
*    MOVE-CORRESPONDING WA_NFE_ACTIVE TO WA_NFE_ALV.
*
*    PERFORM SET_STATUS_ICONS CHANGING WA_NFE_ALV.
*
*    IF SY-TCODE EQ 'ZCTE'.
*      PERFORM SET_STATUS_CIOT CHANGING WA_NFE_ALV.
*    ENDIF.
*
*    IF SY-TCODE EQ 'ZNFE_TERC' OR SY-TCODE EQ 'ZCTE_TERC'.
*      SELECT SINGLE REFKEY
*        FROM J_1BNFLIN
*        INTO VREFKEY
*        WHERE DOCNUM EQ WA_NFE_ACTIVE-DOCNUM.
*
*      SELECT SINGLE *
*        FROM RBKP
*        INTO WRBKP
*        WHERE BELNR EQ VREFKEY.
*
*      SELECT SINGLE *
*          FROM BKPF
*          INTO WBKPF
*          WHERE BUKRS  = WRBKP-BUKRS
*          AND GJAHR  = WRBKP-GJAHR
*          AND AWKEY = VREFKEY.
*
*      SELECT SINGLE AUGDT
*        FROM BSAK
*        INTO VAUGDT
*        WHERE BUKRS  = WBKPF-BUKRS
*        AND   BELNR = WBKPF-BELNR
*        AND   GJAHR = WBKPF-GJAHR.
*
*      MOVE VREFKEY      TO WA_NFE_ALV-REFKEY.
*      MOVE WRBKP-USNAM  TO WA_NFE_ALV-USNAM.
*      MOVE WBKPF-BELNR  TO WA_NFE_ALV-BELNR.
*      MOVE VAUGDT       TO WA_NFE_ALV-AUGDT.
*      WA_NFE_ALV-ZFBDT = WRBKP-ZBD1T + WRBKP-ZFBDT.
*
*      SELECT SINGLE *
*        FROM ZIB_NFE_FORN
*        INTO WZIB_NFE_FORN
*        WHERE NU_CHAVE_CNPJ   =  WA_NFE_ACTIVE-STCD1
*      AND   NU_CHAVE_MODELO =  WA_NFE_ACTIVE-MODEL
*      AND   NU_CHAVE_SERIE  =  WA_NFE_ACTIVE-SERIE
*      AND   NU_CHAVE_NUMERO =  WA_NFE_ACTIVE-NFNUM9.
*
*    ENDIF.
*    IF ST_NOTA IS NOT INITIAL.
*      IF WZIB_NFE_FORN-ST_NOTA IN ST_NOTA.
*        APPEND WA_NFE_ALV TO IT_NFE_ALV.
*      ENDIF.
*    ELSE.
*      APPEND WA_NFE_ALV TO IT_NFE_ALV.
*    ENDIF.
*
*    GF_COL_NUM_TOTAL = GF_COL_NUM_TOTAL + C_1.
*    CLEAR WZIB_NFE_FORN.
*
*ENDSELECT.

ENDFORM.                    " nfe_active_read
*&---------------------------------------------------------------------*
*& Form seleciona_dados_nf
*&---------------------------------------------------------------------*
FORM seleciona_dados_nf  TABLES it_nfe_active_pesq STRUCTURE j_1bnfe_active.



  " Seleciona registros da Tabela j_1bnfe_active.
  PERFORM seleciona_registros_nfe_active TABLES it_nfe_active_pesq.



  IF it_nfe_active_pesq[] IS INITIAL AND
     chave    IS INITIAL             AND
     chavecnf IS INITIAL.

    " Seleciona dados de Nota Fiscal.
    SELECT *
           FROM j_1bnfe_active
           INTO TABLE it_nfe_active_pesq
           WHERE docnum      IN docnum
           AND   docsta      IN docsta
           AND   scssta      IN scssta
           AND   code        IN code
           AND   direct      IN direct
           AND   conting     IN gt_contin
           AND   cancel      IN gt_cancel
           AND   printd      IN gt_printed
           AND   credat      IN date0
           AND   action_date IN date
           AND   stcd1       IN stcd1
           AND   model       IN model
           AND   nfnum9      IN nfnum9
           AND   form        IN form
           AND   serie       IN serie
           AND   nfyear      IN nfyear
           AND   nfmonth     IN nfmont
           AND   bukrs       IN bukrs
           AND   branch      IN bupla
           AND   vstel       IN shipt
           AND   action_user IN user
           AND   crenam      IN usercre
           AND   crenam      NE 'INTERFACE'
           AND   parid       IN partner
           AND   action_requ IN useact
           AND   conting_s   IN gt_contis
           AND   credat      GE dt_posicao
           AND   docnum      IN rgdoref.

  ENDIF.



ENDFORM.
*&---------------------------------------------------------------------*
*& Form seleciona_registros_nfe_active
*&---------------------------------------------------------------------*
FORM seleciona_registros_nfe_active  TABLES it_nfe_active_pesq STRUCTURE j_1bnfe_active.

  DATA: v_ch_nfe(44)  TYPE c,
        vl_lenght_nfe TYPE i.



  CREATE OBJECT zcl_util.

  CLEAR: it_nfe_active_pesq[], t_campos_nfe, t_campos_nfe[].



  "------------------------------"
  " Dados de notas fiscais NFE
  "------------------------------"
  IF chave <> ' ' .


    LOOP AT chave.
      v_ch_nfe = chave-low.
      vl_lenght_nfe = strlen( v_ch_nfe ).
      IF vl_lenght_nfe EQ '44'.
        w_campos_nfe = zcl_util->get_atributos_nfe( v_ch_nfe ).
        APPEND w_campos_nfe TO t_campos_nfe.
      ENDIF.
    ENDLOOP.

    IF NOT t_campos_nfe[] IS INITIAL.


      " Seleciona dados de Nota Fiscal.
      SELECT *
             FROM j_1bnfe_active
             INTO TABLE it_nfe_active_pesq
             FOR ALL ENTRIES IN t_campos_nfe
             WHERE docnum    IN docnum
             AND docsta      IN docsta
             AND scssta      IN scssta
             AND code        IN code
             AND direct      IN direct
             AND conting     IN gt_contin
             AND cancel      IN gt_cancel
             AND printd      IN gt_printed
             AND credat      IN date0
             AND action_date IN date
             AND stcd1       EQ t_campos_nfe-stcd1          "US #107011
             AND model       EQ t_campos_nfe-model          "US #107011
             AND nfnum9      EQ t_campos_nfe-nfnum9         "US #107011
             AND docnum9     EQ t_campos_nfe-docnum9        "US #107011
             AND cdv         EQ t_campos_nfe-cdv            "US #107011
             AND form        IN form
             AND serie       EQ t_campos_nfe-serie          "US #107011
             AND regio       EQ t_campos_nfe-regio          "US #107011
             AND nfyear      EQ t_campos_nfe-nfyear         "US #107011
             AND nfmonth     EQ t_campos_nfe-nfmonth        "US #107011
             AND bukrs       IN bukrs
             AND branch      IN bupla
             AND vstel       IN shipt
             AND action_user IN user
             AND crenam      IN usercre                     "1165360
             AND crenam      NE 'INTERFACE'
             AND parid       IN partner
             AND action_requ IN useact
             AND conting_s   IN gt_contis
             AND credat      GE dt_posicao
             AND docnum      IN rgdoref.

      DELETE it_nfe_active_pesq WHERE model NOT IN model.

    ENDIF.

  ENDIF.



  "------------------------------"
  " Dados de notas fiscais ZCTE
  "------------------------------"
  IF sy-tcode = 'ZCTE'.

    IF NOT chavecnf[] IS INITIAL.

      " Dados Frete Aquaviario
      SELECT *
             FROM zlest0060
             INTO TABLE t_zlest0060
             WHERE chave_nfe IN chavecnf.

      SORT t_zlest0060 BY docnum.

      LOOP AT t_zlest0060 INTO wa_zlest0060.
        wa_documentos-docnum = wa_zlest0060-docnum.
        APPEND wa_documentos TO t_documentos.
      ENDLOOP.

      " Dados Frete Rodoviario
      SELECT *
             FROM zcte_info_nota
             INTO TABLE t_zcte_info_nota
             WHERE nfe   EQ abap_true
             AND   chave IN chavecnf.

      SORT t_zcte_info_nota BY docnum.

      LOOP AT t_zcte_info_nota INTO wa_zcte_info_nota.
        wa_documentos-docnum = wa_zcte_info_nota-docnum.
        APPEND wa_documentos TO t_documentos.
      ENDLOOP.

      IF NOT t_documentos[] IS INITIAL.

        SORT t_documentos BY docnum.

        SELECT *
               FROM j_1bnfe_active
               INTO TABLE it_nfe_active_pesq
               FOR ALL ENTRIES IN t_documentos
               WHERE docnum      EQ t_documentos-docnum
               AND   docsta      IN docsta
               AND   scssta      IN scssta
               AND   code        IN code
               AND   direct      IN direct
               AND   conting     IN gt_contin
               AND   cancel      IN gt_cancel
               AND   printd      IN gt_printed
               AND   credat      IN date0
               AND   action_date IN date
               AND   stcd1       IN stcd1
               AND   model       IN model
               AND   nfnum9      IN nfnum9
               AND   form        IN form
               AND   serie       IN serie
               AND   nfyear      IN nfyear
               AND   nfmonth     IN nfmont
               AND   bukrs       IN bukrs
               AND   branch      IN bupla
               AND   vstel       IN shipt
               AND   action_user IN user
               AND   crenam      IN usercre
               AND   crenam      NE 'INTERFACE'
               AND   parid       IN partner
               AND   action_requ IN useact
               AND   conting_s   IN gt_contis
               AND   credat      GE dt_posicao
               AND   docnum      IN rgdoref.

      ENDIF.

    ENDIF.

  ENDIF.


ENDFORM.
