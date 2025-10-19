FUNCTION z_les_notas_vt.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_VTTK) TYPE  VTTK
*"  EXPORTING
*"     REFERENCE(E_ZLEST0110) TYPE  ZLEST0110
*"  TABLES
*"      IT_ITEM_NOTA STRUCTURE  J_1BNFLIN
*"      IT_ZLEST0110 STRUCTURE  ZLEST0110 OPTIONAL
*"----------------------------------------------------------------------
  TYPES: BEGIN OF y_vbfa,
           vbeln        TYPE vbfa-vbeln,
           posnn        TYPE vbfa-posnn,
           vbeln_35(35) TYPE c,
         END OF y_vbfa,

         BEGIN OF y_vbfa_docm,
           vbeln        TYPE vbfa-vbeln,
           mjahr        TYPE vbfa-mjahr,
           posnn        TYPE vbfa-posnn,
           vbeln_35(35) TYPE c,
         END OF y_vbfa_docm,

         BEGIN OF y_lips,
           vbeln        TYPE lips-vbeln,
           vgbel        TYPE lips-vgbel,
           vgpos        TYPE lips-vgpos,
           werks        TYPE lips-werks,
           vbeln_16(16) TYPE c,
         END OF y_lips,

         BEGIN OF y_ekbe,
           ebeln TYPE ekbe-ebeln,
           ebelp TYPE ekbe-ebelp,
           gjahr TYPE ekbe-gjahr,
           belnr TYPE ekbe-belnr,
           xblnr TYPE ekbe-xblnr,
         END OF y_ekbe,

         BEGIN OF y_rbkp,
           belnr TYPE rbkp-belnr,
           gjahr TYPE rbkp-gjahr,
         END OF y_rbkp,

         BEGIN OF y_reflin,
           refkey(35) TYPE c,
         END OF y_reflin.

  DATA: vl_tabix          TYPE sy-tabix,
        wa_cte_item       LIKE j_1bnflin,
        wa_fatura_servico LIKE vbrp,
        wa_ordem_venda    LIKE vbak,
        it_item_transp    TYPE TABLE OF vttp      WITH HEADER LINE,
        wa_vbfap          TYPE y_vbfa,
        it_vbfap          TYPE TABLE OF y_vbfa    WITH HEADER LINE,
        it_vbfap_docm     TYPE TABLE OF y_vbfa_docm,
        wa_vbfap_docm     TYPE y_vbfa_docm,
        vl_mblnr          TYPE mblnr,
        it_lips           TYPE TABLE OF y_lips    INITIAL SIZE 0 WITH HEADER LINE,
        it_ekbe           TYPE TABLE OF y_ekbe    INITIAL SIZE 0 WITH HEADER LINE,
        it_rbkp           TYPE TABLE OF y_rbkp    INITIAL SIZE 0 WITH HEADER LINE,
        it_reflin         TYPE TABLE OF y_reflin  INITIAL SIZE 0 WITH HEADER LINE,
        wa_rbkp           TYPE y_rbkp,
        wa_reflin         TYPE y_reflin,
        wa_info_part      TYPE lfa1,
        nf_aviso_receb    TYPE TABLE OF zmmt_ee_zgr_docs WITH HEADER LINE,
        wa_kna1           TYPE kna1,
        destinatario_aux  TYPE j_1bnfnad,
        vl_ntgew          TYPE j_1bnfdoc-ntgew,
        vl_gewei          TYPE j_1bnfdoc-gewei,
        tl_lips           TYPE TABLE OF lips,
        wa_lips           TYPE lips,
        tl_ekbe           TYPE TABLE OF ekbe,
        wa_ekbe           TYPE ekbe,
        wa_likp           TYPE likp,
        tl_rbkp           TYPE TABLE OF rbkp,
        wl_rbkp           TYPE rbkp,
        belnr_ano         TYPE c LENGTH 14,
        it_zfiwrt0008     TYPE TABLE OF zfiwrt0008,
        wa_zfiwrt0008     TYPE zfiwrt0008,
        it_j_1bnfdoc      TYPE TABLE OF j_1bnfdoc,
        wa_j_1bnfdoc      TYPE j_1bnfdoc,
        wl_nfe            TYPE c LENGTH 9,
        wl_serie          TYPE c LENGTH 3,
        wl_nfe_serie      TYPE string,
        pos               TYPE i,
        tam               TYPE i,
        it_t001w          TYPE TABLE OF t001w.

  DATA: tabix.

  SELECT * INTO TABLE it_item_transp
    FROM vttp
   WHERE tknum = i_vttk-tknum.

  CHECK sy-subrc IS INITIAL.

  IF i_vttk-abfer = '1' OR   "Transporte de partida com carga
     i_vttk-abfer = '3'.     "Transporte de partida vazio

    IF i_vttk-shtyp = 'Z020'.      "Transferência

      SELECT vbeln mjahr posnn vbeln INTO TABLE it_vbfap_docm
        FROM vbfa
         FOR ALL ENTRIES IN it_item_transp
       WHERE vbelv = it_item_transp-vbeln
         AND vbtyp_v = 'J'
         AND vbtyp_n = 'R'.

      LOOP AT it_vbfap_docm INTO wa_vbfap_docm.
        vl_tabix = sy-tabix.
        CLEAR vl_mblnr.

        SELECT mblnr INTO vl_mblnr
          FROM mseg
         UP TO 1 ROWS
         WHERE smbln = wa_vbfap_docm-vbeln
           AND sjahr = wa_vbfap_docm-mjahr
           AND bwart = '864'.   "Movimento de estorno
        ENDSELECT.

        IF sy-subrc EQ 0.
          DELETE it_vbfap_docm INDEX vl_tabix.
        ELSE.
          CONCATENATE wa_vbfap_docm-vbeln_35 wa_vbfap_docm-mjahr INTO wa_vbfap_docm-vbeln_35.
          MODIFY it_vbfap_docm FROM wa_vbfap_docm INDEX vl_tabix TRANSPORTING vbeln_35.
        ENDIF.
      ENDLOOP.

    IF it_vbfap_docm IS NOT INITIAL.
      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE it_item_nota
        FROM j_1bnflin
         FOR ALL ENTRIES IN it_vbfap_docm
       WHERE refkey = it_vbfap_docm-vbeln_35
         AND refitm = it_vbfap_docm-posnn
         AND reftyp = 'MD'. "Documento de material
    ENDIF.
*---> 04/07/2023 - Migração S4 - WS
  SORT it_item_nota.
*<--- 04/07/2023 - Migração S4 - WS
      DELETE ADJACENT DUPLICATES FROM it_item_nota.

    ELSEIF i_vttk-shtyp = 'Z026'.  "Frete Prestado a Terceiro

    ELSEIF i_vttk-shtyp = 'Z018'.  "Venda Triangular

      SELECT v~vbeln v~posnn v~vbeln INTO TABLE it_vbfap
        FROM vbfa AS v
       INNER JOIN vbrk AS k ON k~vbeln EQ v~vbeln
       INNER JOIN vbrp AS p ON p~vbeln EQ v~vbeln AND p~posnr EQ v~posnn "#EC CI_DB_OPERATION_OK[2768887]
       INNER JOIN vbak AS e ON e~vbeln EQ p~aubel
         FOR ALL ENTRIES IN it_item_transp
       WHERE v~vbelv   = it_item_transp-vbeln
         AND v~vbtyp_v = 'J'   "Entrega
         AND v~vbtyp_n = 'M'   "Fatura
         AND k~fksto  <> 'X'
         AND e~auart   = 'ZREM' AND K~DRAFT = SPACE .

      IF sy-subrc EQ 0.

        SELECT *
          INTO CORRESPONDING FIELDS OF TABLE it_item_nota
          FROM j_1bnflin
           FOR ALL ENTRIES IN it_vbfap
         WHERE refkey = it_vbfap-vbeln_35
           AND refitm = it_vbfap-posnn
           AND reftyp = 'BI'.           "Faturamento

*---> 04/07/2023 - Migração S4 - WS
  SORT it_item_nota.
*<--- 04/07/2023 - Migração S4 - WS
        DELETE ADJACENT DUPLICATES FROM it_item_nota.

      ENDIF.

    ELSEIF i_vttk-shtyp = 'Z009'.  "Devolução de Venda

      "Ordem de Venda de Devolução
      SELECT v~vbelv, v~posnn, v~vbeln INTO TABLE @DATA(it_vbfap_ov_dev)
        FROM vbfa AS v
         FOR ALL ENTRIES IN @it_item_transp
       WHERE v~vbeln   EQ @it_item_transp-vbeln
         AND v~vbtyp_n EQ 'T'
         AND v~vbtyp_v EQ 'H'.

      CHECK sy-subrc IS INITIAL.

      "Fatura da Devolução
      SELECT v~vbelv, v~posnn, v~vbeln INTO TABLE @DATA(it_vbfap_ft_dev)
        FROM vbfa AS v
       INNER JOIN vbrk AS k ON k~vbeln = v~vbeln
         FOR ALL ENTRIES IN @it_vbfap_ov_dev
       WHERE v~vbelv   EQ @it_vbfap_ov_dev-vbelv
         AND v~vbtyp_n EQ 'O'
         AND v~vbtyp_v EQ 'H'
         AND k~fksto   NE 'X' AND K~DRAFT = @SPACE .

      CHECK sy-subrc IS INITIAL.

      LOOP AT it_vbfap_ft_dev INTO DATA(wa_vbfap_ft_dev).
        CLEAR: wa_vbfap.
        wa_vbfap-posnn    = wa_vbfap_ft_dev-posnn.
        wa_vbfap-vbeln    = wa_vbfap_ft_dev-vbeln.
        wa_vbfap-vbeln_35 = wa_vbfap_ft_dev-vbeln.
        APPEND wa_vbfap TO it_vbfap.
      ENDLOOP.

      SELECT * INTO TABLE it_item_nota
        FROM j_1bnflin
         FOR ALL ENTRIES IN it_vbfap
       WHERE refkey = it_vbfap-vbeln_35
         AND refitm = it_vbfap-posnn
         AND reftyp = 'BI'.

*---> 04/07/2023 - Migração S4 - WS
  SORT it_item_nota.
*<--- 04/07/2023 - Migração S4 - WS
      DELETE ADJACENT DUPLICATES FROM it_item_nota.

    ELSE.

      SELECT v~vbeln v~posnn v~vbeln INTO TABLE it_vbfap
        FROM vbfa AS v
       INNER JOIN vbrk AS k ON k~vbeln = v~vbeln
         FOR ALL ENTRIES IN it_item_transp
       WHERE v~vbelv = it_item_transp-vbeln
         AND v~vbtyp_v EQ 'J'
         AND v~vbtyp_n EQ 'M'
         AND k~fksto   NE 'X' AND K~DRAFT = SPACE .

      if it_vbfap[] is NOT INITIAL.
        SELECT * INTO TABLE it_item_nota
          FROM j_1bnflin
           FOR ALL ENTRIES IN it_vbfap
         WHERE refkey = it_vbfap-vbeln_35
           AND refitm = it_vbfap-posnn
           AND reftyp = 'BI'.
      endif.

*---> 04/07/2023 - Migração S4 - WS
  SORT it_item_nota.
*<--- 04/07/2023 - Migração S4 - WS
      DELETE ADJACENT DUPLICATES FROM it_item_nota.

      IF it_item_nota[] IS INITIAL AND ( i_vttk-shtyp = 'Z005' ).
        READ TABLE it_item_transp INDEX 1.

        IF sy-subrc IS INITIAL.
          FREE: it_zlest0110.

          SELECT * INTO CORRESPONDING FIELDS OF TABLE it_zlest0110
            FROM zlest0213
           WHERE vbeln = it_item_transp-vbeln.
        ENDIF.
      ENDIF.

    ENDIF.

*-CS2021001045 - 15.02.2022 - JT - inicio
  ELSEIF ( i_vttk-abfer = '2' AND i_vttk-shtyp = 'Z021' ).
*-CS2021001045 - 15.02.2022 - JT - fim

    READ TABLE it_item_transp INDEX 1.

    IF sy-subrc IS INITIAL.

      CLEAR: e_zlest0110.

      SELECT SINGLE * INTO e_zlest0110
        FROM zlest0110
       WHERE vbeln = it_item_transp-vbeln.

      IF sy-subrc IS INITIAL.

        SELECT SINGLE * INTO  @DATA(wa_zlest0108)
          FROM zlest0108
         WHERE vbeln EQ @it_item_transp-vbeln.

        IF sy-subrc IS INITIAL AND wa_zlest0108-id_agrupa_frete IS NOT INITIAL.
          SELECT * INTO TABLE @DATA(it_zlest0108)
            FROM zlest0108
           WHERE id_agrupa_frete EQ @wa_zlest0108-id_agrupa_frete.

          SELECT * INTO TABLE @it_zlest0110
            FROM zlest0110
             FOR ALL ENTRIES IN @it_zlest0108
            WHERE vbeln EQ @it_zlest0108-vbeln.
        ELSE.
          APPEND e_zlest0110 TO it_zlest0110.
        ENDIF.

      ENDIF.

      SELECT * INTO TABLE nf_aviso_receb
        FROM zmmt_ee_zgr_docs
       WHERE av_vbeln EQ it_item_transp-vbeln.

      DELETE nf_aviso_receb WHERE docnum EQ space.

      IF nf_aviso_receb[] IS NOT INITIAL.
        SELECT * INTO TABLE it_item_nota
          FROM j_1bnflin
          FOR ALL ENTRIES IN nf_aviso_receb
         WHERE docnum EQ nf_aviso_receb-docnum.
      ELSEIF ( e_zlest0110 IS INITIAL ). " Se não tiver Aviso de Recebimento pela Transação ZLES0113

*          SELECT SINGLE * FROM LIPS INTO WA_LIPS WHERE VBELN EQ IT_ITEM_TRANSP-VBELN
*                                                   AND POSNR EQ '000010'.

        SELECT * FROM lips INTO TABLE tl_lips
          WHERE vbeln EQ it_item_transp-vbeln.


        IF ( sy-subrc EQ 0 ).

          SELECT SINGLE * FROM likp INTO wa_likp WHERE vbeln EQ it_item_transp-vbeln.

          READ TABLE tl_lips INTO wa_lips INDEX 1.

          SELECT SINGLE * FROM ekko INTO @DATA(wl_ekko) WHERE ebeln EQ @wa_lips-vgbel.

          IF ( sy-subrc = 0 ) AND ( wl_ekko-bsart EQ 'ZGEF' ). "Entrega Futura

            DATA(_achou_nf) = ''.

            SELECT * FROM ekbe INTO TABLE tl_ekbe WHERE ebeln EQ wa_lips-vgbel
                                                    AND vgabe EQ '1'.

            LOOP AT tl_ekbe INTO wa_ekbe. "Percorrer Historico pedido

              CLEAR: it_item_nota[], wa_j_1bnfdoc, belnr_ano, wl_nfe, wl_serie, wl_nfe_serie.

              SELECT SINGLE *
                FROM mseg INTO @DATA(wl_mseg)
               WHERE sjahr = @wa_ekbe-gjahr
                 AND smbln = @wa_ekbe-belnr.

              CHECK sy-subrc NE 0. "Não estornado

              CONCATENATE wa_ekbe-belnr wa_ekbe-gjahr INTO belnr_ano.

              CHECK belnr_ano IS NOT INITIAL.

              SELECT * INTO TABLE it_item_nota
                FROM j_1bnflin
               WHERE refkey EQ belnr_ano.

              CHECK it_item_nota[] IS NOT INITIAL.

              READ TABLE it_item_nota INTO DATA(wl_item_nota) INDEX 1.

              CHECK sy-subrc = 0.

              SELECT SINGLE *
                FROM j_1bnfdoc INTO wa_j_1bnfdoc
               WHERE docnum = wl_item_nota-docnum
                 AND candat = '00000000'
                 AND doctyp NE '5'.

              CHECK sy-subrc EQ 0.

              IF wa_j_1bnfdoc-nfe = 'X'.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = wa_j_1bnfdoc-nfenum
                  IMPORTING
                    output = wl_nfe.
              ELSE.
                CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                  EXPORTING
                    input  = wa_j_1bnfdoc-nfnum
                  IMPORTING
                    output = wl_nfe.
              ENDIF.

              CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                EXPORTING
                  input  = wa_j_1bnfdoc-series
                IMPORTING
                  output = wl_serie.

              CONCATENATE wl_nfe '-' wl_serie INTO wl_nfe_serie.

              IF wl_nfe_serie EQ wa_likp-lifex.
                _achou_nf = 'X'.
                EXIT.
              ELSE.
                CLEAR: it_item_nota[].
              ENDIF.

            ENDLOOP. "LOOP AT TL_EKBE INTO WA_EKBE.

            IF _achou_nf = ''.
              CLEAR: it_item_nota[].
            ENDIF.

          ENDIF. "IF ( SY-SUBRC = 0 ) AND ( WL_EKKO-BSART EQ 'ZGEF' ).

          SELECT * FROM ekbe INTO TABLE tl_ekbe WHERE ebeln EQ wa_lips-vgbel
                                                  AND vgabe EQ '2'.
          "AND GJAHR EQ WA_LIPS-ERDAT(4).

          IF ( sy-subrc = 0 ) AND ( _achou_nf IS INITIAL ).

            SELECT * FROM rbkp INTO TABLE tl_rbkp
              FOR ALL ENTRIES IN tl_ekbe
            WHERE belnr EQ tl_ekbe-belnr
              AND gjahr EQ tl_ekbe-gjahr
              AND stblg EQ ''.

            IF ( sy-subrc EQ 0 ).

              LOOP AT tl_rbkp INTO wl_rbkp.
                tabix = sy-tabix.
                IF ( wl_rbkp-xblnr NE wa_likp-lifex ).
                  DELETE tl_rbkp INDEX tabix.
                ENDIF.
              ENDLOOP.

              CLEAR: wl_rbkp.
              READ TABLE tl_rbkp INTO wl_rbkp INDEX 1.

              IF ( sy-subrc EQ 0 ).

                CONCATENATE wl_rbkp-belnr wl_rbkp-gjahr INTO belnr_ano.

                IF NOT ( belnr_ano IS INITIAL ).
                  SELECT * INTO TABLE it_item_nota
                   FROM j_1bnflin
                  WHERE refkey EQ belnr_ano.
                ENDIF.

              ELSE.

                SELECT * FROM zfiwrt0008
                  INTO TABLE it_zfiwrt0008
                  FOR ALL ENTRIES IN tl_lips
                WHERE ebeln EQ tl_lips-vgbel.

                CHECK NOT it_zfiwrt0008[] IS INITIAL.

                SELECT * FROM j_1bnfdoc
                  INTO TABLE it_j_1bnfdoc
                  FOR ALL ENTRIES IN it_zfiwrt0008
                WHERE docnum EQ it_zfiwrt0008-docnum.

                IF ( sy-subrc EQ 0 ).

                  LOOP AT it_j_1bnfdoc INTO wa_j_1bnfdoc.
                    tabix = sy-tabix.

                    CLEAR: wl_nfe, wl_serie, wl_nfe_serie.

                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                      EXPORTING
                        input  = wa_j_1bnfdoc-nfenum
                      IMPORTING
                        output = wl_nfe.


                    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                      EXPORTING
                        input  = wa_j_1bnfdoc-series
                      IMPORTING
                        output = wl_serie.


                    CONCATENATE wl_nfe '-' wl_serie INTO wl_nfe_serie.

                    IF ( wl_nfe_serie NE wa_likp-lifex ).
                      DELETE it_j_1bnfdoc INDEX tabix.
                    ENDIF.
                    CLEAR: wa_j_1bnfdoc, tabix.
                  ENDLOOP.

                  IF  NOT ( it_j_1bnfdoc[] IS INITIAL ).
                    SELECT * INTO TABLE it_item_nota
                     FROM j_1bnflin
                    FOR ALL ENTRIES IN it_j_1bnfdoc
                    WHERE docnum EQ it_j_1bnfdoc-docnum.
                  ENDIF.

                ENDIF.
              ENDIF.
            ENDIF.

          ELSEIF ( _achou_nf IS INITIAL ).

            CLEAR: tam, pos, wl_nfe, wl_nfe_serie.
            REFRESH: it_j_1bnfdoc[], it_item_nota[].

            tam = strlen( wa_likp-lifex ).
            FIND '-' IN wa_likp-lifex RESPECTING CASE MATCH OFFSET pos.
            wl_nfe = wa_likp-lifex(pos).
            pos = pos + 1.
            tam = ( tam - pos ).
            wl_nfe_serie = wa_likp-lifex+pos(tam).

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wl_nfe
              IMPORTING
                output = wl_nfe.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = wl_nfe_serie
              IMPORTING
                output = wl_nfe_serie.

            SELECT * FROM j_1bnfdoc
              INTO TABLE it_j_1bnfdoc
            WHERE parid  EQ wa_likp-lifnr
              AND nfenum EQ wl_nfe
              AND series EQ wl_nfe_serie.


            IF ( sy-subrc EQ 0 ).
              SELECT * INTO TABLE it_item_nota
               FROM j_1bnflin
              FOR ALL ENTRIES IN it_j_1bnfdoc
              WHERE docnum EQ it_j_1bnfdoc-docnum.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.

      SELECT vbeln vgbel vgpos werks vbeln INTO TABLE it_lips
        FROM lips
         FOR ALL ENTRIES IN it_item_transp
       WHERE vbeln = it_item_transp-vbeln.

      CHECK sy-subrc EQ 0.

      SELECT * INTO TABLE it_t001w
        FROM t001w
        FOR ALL ENTRIES IN it_lips
       WHERE werks EQ it_lips-werks.

      SELECT ebeln ebelp gjahr belnr xblnr INTO TABLE it_ekbe
        FROM ekbe
         FOR ALL ENTRIES IN it_lips
       WHERE ebeln = it_lips-vgbel
         AND ebelp = it_lips-vgpos+1(5)
         AND xblnr = it_lips-vbeln_16
         AND bewtp = 'Q'.

      SELECT belnr gjahr INTO TABLE it_rbkp
        FROM rbkp
         FOR ALL ENTRIES IN it_ekbe
       WHERE belnr = it_ekbe-belnr
         AND stblg = space.

      LOOP AT it_rbkp INTO wa_rbkp.
        CONCATENATE wa_rbkp-belnr wa_rbkp-gjahr INTO wa_reflin-refkey.
        APPEND wa_reflin TO it_reflin.
      ENDLOOP.

      SELECT *
        INTO CORRESPONDING FIELDS OF TABLE it_item_nota
        FROM j_1bnflin
         FOR ALL ENTRIES IN it_reflin
       WHERE reftyp = 'LI'           "Logística: Revisão de faturas
         AND refkey = it_reflin-refkey.

    ENDIF.

    SORT it_item_nota BY docnum.
    DELETE ADJACENT DUPLICATES FROM it_item_nota COMPARING docnum.

  ENDIF.

ENDFUNCTION.
