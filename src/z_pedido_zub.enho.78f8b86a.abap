"Name: \FU:J_1B_IM_TF_GET_GI_TXCD\SE:END\EI
ENHANCEMENT 0 Z_PEDIDO_ZUB.
*
  DATA: t_grupo      TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
        wa_nf_lin    LIKE j_1bnflin OCCURS 0 WITH HEADER LINE,
        regio_e      TYPE lfa1-regio,
        regio_d      TYPE lfa1-regio,
        vwerks_e     TYPE lfa1-lifnr,
        vwerks_d     TYPE lfa1-lifnr,
        pos          TYPE i,
        v_forn_orig  TYPE lfa1-lifnr,
        v_forn_dest  TYPE lfa1-lifnr,
        v_ownpr      TYPE mbew-ownpr,
        v_mtorg      TYPE mbew-mtorg, "US #154447 - MMSILVA - 02.04.2025
        v_matkl      TYPE mara-matkl,
        v_regio_orig TYPE lfa1-regio,
        v_regio_dest TYPE lfa1-regio,
        lc_dados     TYPE zsde0185,    "*-US191846-01.10.2025-#191846-JT-inicio
        lc_retorno   TYPE zmmt0154_t,  "*-US191846-01.10.2025-#191846-JT-inicio
        wc_retorno   TYPE zmmt0154.    "*-US191846-01.10.2025-#191846-JT-inicio

  DATA wa_zmmt0154 TYPE zmmt0154.



  IF is_mseg-bwart = '862'.
*&==============================================================Comentado/IR181421/aoenning
*  IF is_mseg-bwart = '862' AND is_mseg-bukrs = '0015'.
*    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
*      EXPORTING
*        class         = '0000'
*        setnr         = 'MAGGI_PEDIDO_ZUB'
*      TABLES
*        set_values    = t_grupo
*      EXCEPTIONS
*        set_not_found = 1
*        OTHERS        = 2.
*    IF sy-subrc <> 0.
**             MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**                     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    ENDIF.
*    SELECT SINGLE matkl
*      FROM mara
*      INTO @DATA(v_matkl)
*      WHERE matnr = @is_mseg-matnr.
*
*    READ TABLE t_grupo WITH KEY from = v_matkl.
*    IF sy-subrc = 0.
*      es_mseg-mwskz = 'ZA'.
*    ENDIF.
*  ELSEIF is_mseg-bwart = '862' AND is_mseg-bukrs = '0001'.
*    SELECT COUNT(*)
*           FROM tvarvc
*            WHERE name = 'MAGGI_BIODIESEL'
*            AND   low  = is_mseg-matnr.
*    IF sy-subrc = 0.
*      es_mseg-mwskz = 'V0'.
*    ELSE.
*      SELECT SINGLE *
*        FROM ekpa
*        INTO @DATA(w_ekpa)
*        WHERE ebeln = @is_mseg-ebeln
*        AND   parvw = 'PR'.
*      "
*      IF sy-subrc = 0.
*        SELECT SINGLE regio
*          FROM lfa1
*          INTO @DATA(w_regio_ori)
*          WHERE lifnr = @w_ekpa-lifn2.
*
*        IF sy-subrc = 0.
*          SELECT SINGLE *
*            FROM ekpo
*            INTO @DATA(w_ekpo)
*            WHERE ebeln = @is_mseg-ebeln
*            AND   ebelp = @is_mseg-ebelp.
*
*          SELECT SINGLE *
*            FROM setleaf
*            INTO @DATA(w_setleaf)
*            WHERE setname = 'MAGGI_CFOP_PEDTRANSF'
*            AND   valfrom = @w_ekpo-werks.
*          IF sy-subrc NE 0.
*            SELECT SINGLE regio
*              FROM t001w
*               INTO @DATA(w_regio_des)
*              WHERE werks = @w_ekpo-werks.
*
*            IF w_regio_ori NE w_regio_des.
*              es_mseg-mwskz = 'ZA'.
*            ELSE.
*              SELECT SINGLE *
*                FROM setleaf
*                INTO @DATA(w_setleaf2)
*                WHERE setname = 'MAGGI_CFOP_PEDTRANSF2'
*                AND   valfrom = @w_ekpo-werks.
*              IF sy-subrc = 0.
*                SELECT SINGLE matkl
*                   FROM mara
*                   INTO @DATA(v_matkl2)
*                   WHERE matnr = @is_mseg-matnr.
*                IF v_matkl2 = '700150'.
*                  es_mseg-mwskz = 'ZA'.
*                ENDIF.
*
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*
*      ENDIF.
*    ENDIF.
*&==============================================================Comentado/IR181421/aoenning

*&==============================================================Inicio ajuste/IR181421/aoenning
    IF is_mseg-ebeln IS NOT INITIAL.
      SELECT SINGLE *
           INTO @DATA(w_ekko)
           FROM ekko
           WHERE ebeln = @is_mseg-ebeln.

*    IF w_ekko-bsart = 'ZUB'  .
*      SELECT COUNT(*)
*         FROM tvarvc
*          WHERE name = 'MAGGI_BIODIESEL'
*          AND   low  = is_mseg-matnr.
*      IF sy-subrc = 0.
*        LOOP AT wa_nf_stx.
*          wa_nf_stx-rate = 0.
*          MODIFY wa_nf_stx.
*        ENDLOOP.
*      ENDIF.
*    ENDIF.

      IF w_ekko-bsart = 'ZUB'  .
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = w_ekko-reswk
          IMPORTING
            output = v_forn_orig.

      ELSE.
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = w_ekko-lifnr
          IMPORTING
            output = v_forn_orig.

      ENDIF.

      IF 'ZUB_ZARM_ZARS' CS w_ekko-bsart AND w_ekko-bsart is NOT INITIAL.
        SELECT SINGLE ekpo~werks
        INTO vwerks_d
         FROM ekpo
         WHERE ekpo~ebeln = is_mseg-ebeln.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = vwerks_d
          IMPORTING
            output = v_forn_dest.

        "Selecionar MBEW por BWKEY = W_EKKO-WERKS e MATNR = wa_nf_lin-matnr obter OWNPR armazenar em V_OWNPR
        SELECT SINGLE ownpr mtorg FROM mbew INTO ( v_ownpr, v_mtorg ) WHERE bwkey = is_mseg-werks AND  matnr = is_mseg-matnr. "US #154447 - MMSILVA - 02.04.2025 - Acrescentado "matorg" e "v_matorg"

        "selecionar MARA por  MATNR = wa_nf_lin-matnr obter MATKL e armzenar em V_MATKL
        SELECT SINGLE matkl FROM mara INTO v_matkl WHERE matnr = is_mseg-matnr.

        "seleionar  LFA1 por LIFNR = V_FORN_ORIG obter REGIO e armazenar em V_REGIO_ORIG
        SELECT SINGLE regio FROM lfa1 INTO v_regio_orig WHERE lifnr = v_forn_orig.

        "seleionar  LFA1 por LIFNR = V_FORN_DEST obter REGIO e armazenar em V_REGIO_DEST
        SELECT SINGLE regio FROM lfa1 INTO v_regio_dest WHERE lifnr = v_forn_dest.

        DATA(lv_matnr)  = is_mseg-matnr.

        CALL FUNCTION 'CONVERSION_EXIT_MATN1_OUTPUT'
          EXPORTING
            input  = is_mseg-matnr
          IMPORTING
            output = lv_matnr.

*-US191846-01.10.2025-#191846-JT-inicio
*        SELECT SINGLE * FROM  zmmt0154 INTO wa_zmmt0154 WHERE
*         bsart       = w_ekko-bsart
*         AND uf_orig   =  v_regio_orig
*         AND uf_dest   =  v_regio_dest
*         AND ownpr    = v_ownpr
*         AND matnr     =  lv_matnr
*         AND mtorg     =  v_mtorg. "US #154447 - MMSILVA - 02.04.2025
** ----> US #154447 - MMSILVA - 02.04.2025 - Inicio - Caso MTORG esteja vazio na tabela
*        IF sy-subrc IS NOT INITIAL.
*          SELECT SINGLE * FROM  zmmt0154 INTO wa_zmmt0154
*            WHERE bsart   = w_ekko-bsart
*            AND uf_orig   = v_regio_orig
*            AND uf_dest   = v_regio_dest
*            AND ownpr     = v_ownpr
*            AND matnr     = lv_matnr.
** ----> US #154447 - MMSILVA - 02.04.2025 - Fim - Caso MTORG esteja vazio na tabela
*          IF sy-subrc IS NOT INITIAL.
*            SELECT SINGLE * FROM  zmmt0154 INTO wa_zmmt0154 WHERE
*                          bsart       = w_ekko-bsart
*                          AND uf_orig   =  v_regio_orig
*                          AND uf_dest   =  v_regio_dest
*                          AND ownpr    =  v_ownpr
*                          AND matkl      = v_matkl
*                          AND mtorg     =  v_mtorg. "US #154447 - MMSILVA - 02.04.2025
** --------> US #154447 - MMSILVA - 02.04.2025 - Inicio - Caso MTORG esteja vazio na tabela
*            IF sy-subrc IS NOT INITIAL.
*              SELECT SINGLE * FROM  zmmt0154 INTO wa_zmmt0154
*                WHERE bsart   = w_ekko-bsart
*                AND uf_orig   = v_regio_orig
*                AND uf_dest   = v_regio_dest
*                AND ownpr     = v_ownpr
*                AND matkl     = v_matkl.
** --------> US #154447 - MMSILVA - 02.04.2025 - Fim - Caso MTORG esteja vazio na tabela
*              IF wa_zmmt0154-mwskz IS NOT INITIAL.
*                CONCATENATE 'Tipo de pedido sem parâmetro fiscal. Por gentileza criar uma FI ao departamento fiscal CHAVE: ' w_ekko-bsart v_regio_orig v_regio_dest v_ownpr is_mseg-matnr INTO DATA(mensagem).
*                MESSAGE: mensagem TYPE 'E'.
*              ENDIF.
*
*            ENDIF.
*
*            IF wa_zmmt0154-mwskz IS NOT INITIAL.
*              es_mseg-mwskz       = wa_zmmt0154-mwskz.
*            ENDIF.
*          ENDIF.
*        ENDIF.

        CLEAR: lc_dados, wa_zmmt0154.
        lc_dados-bsart-valor   = w_ekko-bsart.
        lc_dados-uf_orig-valor = v_regio_orig.
        lc_dados-uf_dest-valor = v_regio_dest.
        lc_dados-matnr-valor   = lv_matnr.
        lc_dados-werks-valor   = is_mseg-werks.
        lc_dados-ownpr-valor   = v_ownpr.
        lc_dados-mtorg-valor   = v_mtorg.
        lc_dados-direcao-valor = '2'. "saida

        lc_retorno = zcl_leis_fiscais=>get_impostos( i_dados = lc_dados i_todos = abap_false ).

        READ TABLE lc_retorno INTO wa_zmmt0154 INDEX 1.

        IF NOT ( sy-subrc = 0 AND wa_zmmt0154-mwskz IS NOT INITIAL ).
          DATA: lva_msg_error TYPE string.

          lva_msg_error = |Não encontrado parametros com IVA na ZMM0185 para o tipo Pedido: { w_ekko-bsart } Origem: { v_regio_orig }|.
          lva_msg_error = |{ lva_msg_error } Destino: { v_regio_dest } Material: { lv_matnr } Produção Interna: { v_ownpr } Origem Material: { v_mtorg } Direção: Saida!|.
          lva_msg_error = |{ lva_msg_error } Criar FI para Departamento Fiscal|.
          MESSAGE lva_msg_error TYPE 'E'.

          "CONCATENATE 'Tipo de pedido sem parâmetro fiscal. Por gentileza criar uma FI ao departamento fiscal CHAVE: ' w_ekko-bsart v_regio_orig v_regio_dest v_ownpr is_mseg-matnr INTO DATA(mensagem).
          "MESSAGE: mensagem TYPE 'E'.
        ELSE.
          es_mseg-mwskz  = wa_zmmt0154-mwskz.
        ENDIF.
*-US191846-01.10.2025-#191846-JT-fim

      ENDIF.
    ENDIF.
  ENDIF.
ENDENHANCEMENT.
