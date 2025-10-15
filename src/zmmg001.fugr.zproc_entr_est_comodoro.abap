FUNCTION zproc_entr_est_comodoro.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_PROC_JOB) TYPE  ZMMT0122-PROC_JOB
*"     REFERENCE(P_DT_MOVIMENTO) TYPE  ZSDT0001-DT_MOVIMENTO OPTIONAL
*"     REFERENCE(P_WERKS) TYPE  ZMMT0122-WERKS OPTIONAL
*"  EXPORTING
*"     REFERENCE(_XDOCMAT309) TYPE  MBLNR
*"     REFERENCE(_XDOCMAT315) TYPE  MBLNR
*"     REFERENCE(_XDOCMAT315_Q) TYPE  MBLNR
*"     REFERENCE(_XERRO) TYPE  CHAR20
*"----------------------------------------------------------------------

  DATA: it_zmmt0122       TYPE TABLE OF zmmt0122,
        wa_zmmt0122       TYPE zmmt0122,
        wa_t001k          TYPE  t001k,
        it_zsdt0001       TYPE TABLE OF  zsdt0001,
        wa_zsdt0001       TYPE zsdt0001,
        wa_j_1bnfe_active TYPE j_1bnfe_active,
        wa_zfiwrt0008     TYPE zfiwrt0008,
        wa_zfiwrt0004     TYPE zfiwrt0004,
        wa_zfiwrt0009     TYPE zfiwrt0009,
        wa_j_1bnflin      TYPE j_1bnflin,
        wa_mara           TYPE mara,
        wa_mara2          TYPE mara,
        wa_zmmt0123       TYPE zmmt0123,
        vg_matnr          TYPE char18,
        p_erro            TYPE char01.

  DATA: xregio       TYPE j_1bnfe_active-regio,
        xnfyear      TYPE j_1bnfe_active-nfyear,
        xnfmonth     TYPE j_1bnfe_active-nfmonth,
        xstcd1       TYPE j_1bnfe_active-stcd1,
        xmodel       TYPE j_1bnfe_active-model,
        xserie       TYPE j_1bnfe_active-serie,
        xnfnum9      TYPE j_1bnfe_active-nfnum9,
        xdocnum9     TYPE j_1bnfe_active-docnum9,
        xcdv         TYPE j_1bnfe_active-cdv,
        xdocmat315   TYPE mblnr,
        wl_headret   TYPE  bapi2017_gm_head_ret,
* Início - Sara - CS2020000884  - Agosto/2020
        xdocmat315_q TYPE mblnr,
* Fim - Sara - CS2020000884  - Agosto/2020
        xdocmat309   TYPE mblnr,
        dt_mov_ent   TYPE zdt_mov,
        _parid       TYPE zsdt0001-parid,
        _normt       TYPE matnr.


  DATA: wl_header  TYPE bapi2017_gm_head_01,
        wl_code    TYPE bapi2017_gm_code,
        wl_mblnr   TYPE zfiwrt0008-mblnr,
        wl_mjahr   TYPE mjahr,
        tl_item    TYPE TABLE OF bapi2017_gm_item_create WITH HEADER LINE,
        tl_return  TYPE TABLE OF bapiret2 WITH HEADER LINE,
        tl_return2 TYPE TABLE OF bapiret2 WITH HEADER LINE.

* Início - Sara - CS2020000884  - Agosto/2020
  DATA:  lv_peso_quebra  TYPE erfmg.
* Fim - Sara - CS2020000884  - Agosto/2020

  CLEAR: wl_header, wl_code, tl_item, wl_mjahr, wl_mblnr, xdocmat315_q, lv_peso_quebra, p_erro.
  FREE: tl_return[], tl_item[].


  IF p_proc_job EQ abap_true.

    FREE: it_zmmt0122.
    SELECT *
      FROM zmmt0122 INTO TABLE it_zmmt0122
      WHERE proc_job EQ p_proc_job.

    LOOP AT it_zmmt0122 INTO wa_zmmt0122.

      CLEAR: wa_t001k.
      SELECT SINGLE * FROM t001k INTO wa_t001k
        WHERE bwkey EQ wa_zmmt0122-werks.

      dt_mov_ent = ( sy-datum - wa_zmmt0122-qte_dias_proc ).

      _parid = |{ wa_zmmt0122-werks ALPHA = IN }|.

      FREE: it_zsdt0001.
      SELECT * FROM zsdt0001 INTO TABLE it_zsdt0001
        WHERE dt_movimento    BETWEEN dt_mov_ent AND sy-datum
        AND   tp_movimento    EQ 'E'
        AND   bukrs           EQ wa_t001k-bukrs
        AND   branch          EQ wa_zmmt0122-werks
        AND   parid           EQ _parid
        AND  (  doc_material  EQ ' '  OR  doc_material_e EQ ' ' ).

      LOOP AT it_zsdt0001 INTO wa_zsdt0001.

        xregio   = wa_zsdt0001-chave_nfe+0(2).
        xnfyear  = wa_zsdt0001-chave_nfe+2(2).
        xnfmonth = wa_zsdt0001-chave_nfe+4(2).
        xstcd1   = wa_zsdt0001-chave_nfe+6(14).
        xmodel   = wa_zsdt0001-chave_nfe+20(2).
        xserie   = wa_zsdt0001-chave_nfe+22(3).
        xnfnum9  = wa_zsdt0001-chave_nfe+25(9).
        xdocnum9 = wa_zsdt0001-chave_nfe+34(9).
        xcdv     = wa_zsdt0001-chave_nfe+43(1).

        CLEAR: wa_j_1bnfe_active.
        SELECT SINGLE * FROM j_1bnfe_active INTO wa_j_1bnfe_active
          WHERE docsta  EQ 1
          AND   regio   EQ xregio
          AND   nfyear  EQ xnfyear
          AND   nfmonth EQ xnfmonth
          AND   stcd1   EQ xstcd1
          AND   model   EQ xmodel
          AND   serie   EQ xserie
          AND   nfnum9  EQ xnfnum9
          AND   docnum9 EQ xdocnum9
          AND   cdv     EQ xcdv
*-CS1105820-#RIMINI-20.06.2023-BEGIN
          AND   cancel  EQ space.
*-CS1105820-#RIMINI-20.06.2023-END
        IF sy-subrc EQ 0.

          CLEAR: wa_j_1bnflin.
          SELECT SINGLE * FROM j_1bnflin INTO wa_j_1bnflin
            WHERE docnum EQ wa_j_1bnfe_active-docnum.

          IF sy-subrc = 0.

            CLEAR: wa_zfiwrt0008.
            SELECT SINGLE * FROM zfiwrt0008 INTO wa_zfiwrt0008  WHERE seq_lcto  EQ wa_j_1bnflin-refkey AND move_stloc EQ wa_zmmt0122-lgort.
            IF sy-subrc NE 0.
              CONTINUE.
            ENDIF.

*            IF SY-SUBRC = 0.

            CLEAR: wa_zfiwrt0004.
            SELECT SINGLE * FROM  zfiwrt0004 INTO wa_zfiwrt0004  WHERE operacao EQ  wa_zfiwrt0008-operacao.

            IF wa_zfiwrt0004-bwart NE wa_zmmt0122-tp_mov_sd.
*--> CS1002304 --->
              wa_zmmt0123-mandt           = sy-mandt.
              wa_zmmt0123-werks           = wa_zmmt0122-werks.
              wa_zmmt0123-lgort           = wa_zmmt0122-lgort.
              wa_zmmt0123-nfenum          = wa_j_1bnfe_active-nfnum9.
              wa_zmmt0123-dt_atualizacao  = sy-datum.
              wa_zmmt0123-type            = 'E'.
              wa_zmmt0123-id              = 'tabela zfiwrt0004'.
              wa_zmmt0123-message         = 'NFe:Verifique cod.operacao e tipo movimento-zfiwrt0004'.
              MODIFY  zmmt0123 FROM wa_zmmt0123.
              CLEAR: wa_zmmt0123, wa_return.
*--> CS1002304 --->
              p_erro = 'X'.
              CONTINUE.
            ENDIF.

*            ENDIF.

            CLEAR: wa_zfiwrt0009.
            SELECT SINGLE * FROM zfiwrt0009 INTO wa_zfiwrt0009  WHERE seq_lcto  EQ  wa_j_1bnflin-refkey.

            IF sy-subrc = 0.

              "Converte material 18 posições.
              CLEAR: vg_matnr.
              wa_zfiwrt0009-matnr = |{ wa_zfiwrt0009-matnr ALPHA = OUT }|.
              vg_matnr = wa_zfiwrt0009-matnr.
              vg_matnr = |{ vg_matnr ALPHA = IN }|.
              CLEAR: wa_zfiwrt0009-matnr.
              wa_zfiwrt0009-matnr = vg_matnr.

              CLEAR: wa_mara.
              SELECT SINGLE * FROM mara INTO wa_mara  WHERE matnr EQ  wa_zfiwrt0009-matnr.

              IF wa_mara-normt IS INITIAL.
*--> CS1002304 --->
                wa_zmmt0123-mandt           = sy-mandt.
                wa_zmmt0123-werks           = wa_zmmt0122-werks.
                wa_zmmt0123-lgort           = wa_zmmt0122-lgort.
                wa_zmmt0123-nfenum          = wa_j_1bnfe_active-nfnum9.
                wa_zmmt0123-dt_atualizacao  = sy-datum.
                wa_zmmt0123-type            = 'E'.
                wa_zmmt0123-id              = 'tabela zfiwrt0009'.
                wa_zmmt0123-message         = 'Verifique Denominação da norma (p.ex. DIN) do material'.
                MODIFY  zmmt0123 FROM wa_zmmt0123.
                CLEAR: wa_zmmt0123, wa_return.
*--> CS1002304 --->
                p_erro = 'X'.
                CONTINUE.
              ELSE.

                _normt = |{ wa_mara-normt ALPHA = IN }|.

                CLEAR: wa_mara2.

                "Converte material 18 posições.
                CLEAR: vg_matnr.
                _normt = |{ _normt ALPHA = OUT }|.
                vg_matnr = _normt.
                vg_matnr = |{ vg_matnr ALPHA = IN }|.
                CLEAR: _normt.
                _normt = vg_matnr.


                SELECT SINGLE * FROM mara INTO wa_mara2  WHERE matnr EQ  _normt.

                IF wa_mara2-mtart NE 'ZROH' AND wa_mara2-mtart NE 'ZHAW'.
*--> CS1002304 --->
                  wa_zmmt0123-mandt           = sy-mandt.
                  wa_zmmt0123-werks           = wa_zmmt0122-werks.
                  wa_zmmt0123-lgort           = wa_zmmt0122-lgort.
                  wa_zmmt0123-nfenum          = wa_j_1bnfe_active-nfnum9.
                  wa_zmmt0123-dt_atualizacao  = sy-datum.
                  wa_zmmt0123-type            = 'E'.
                  wa_zmmt0123-id              = ''.
                  wa_zmmt0123-message         = 'Verifique Tipo de material'.
                  MODIFY  zmmt0123 FROM wa_zmmt0123.
                  CLEAR: wa_zmmt0123, wa_return.
                  p_erro = 'X'.
*--> CS1002304 --->
                  CONTINUE.
                ENDIF.

              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.

        IF wa_zsdt0001-doc_material_e EQ ' ' AND wa_zmmt0122-tp_mov_et NE ' '.

          wl_header-pstng_date  = wa_zsdt0001-dt_movimento.
          wl_header-doc_date    = wa_zsdt0001-dt_movimento.
          wl_header-header_txt  = wa_j_1bnfe_active-nfnum9.

          wl_code-gm_code       = '06'.


*---> 06/07/2023 - Migração S4 - MA
*        tl_item-material      = wa_zfiwrt0009-matnr.


          DATA(v_len) = strlen( wa_zfiwrt0009-matnr ).

          IF v_len > 18.
            tl_item-material_long = wa_zfiwrt0009-matnr.
          ELSE.
            tl_item-material       = wa_zfiwrt0009-matnr.
          ENDIF.
*<--- 06/07/2023 - Migração S4 - MA
          tl_item-plant	        =	wa_zsdt0001-branch.
          tl_item-stge_loc      = wa_zfiwrt0008-move_stloc.
          tl_item-batch         = wa_zfiwrt0009-charg.
          tl_item-move_type     = wa_zmmt0122-tp_mov_et.
* Início - Sara - CS2020000884  - Agosto/2020
          tl_item-entry_qnt     = wa_zfiwrt0009-menge.
*          tl_item-entry_qnt     = wa_zsdt0001-peso_liq.
* Fim - Sara - CS2020000884  - Agosto/2020

          APPEND tl_item.

          FREE: tl_return.
          CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              goodsmvt_header  = wl_header
              goodsmvt_code    = wl_code
            IMPORTING
              materialdocument = wl_mblnr
              matdocumentyear  = wl_mjahr
            TABLES
              goodsmvt_item    = tl_item
              return           = tl_return.

          IF sy-subrc IS INITIAL.

            LOOP AT tl_return  INTO wa_return  WHERE type EQ 'E' .
              wa_zmmt0123-mandt           = sy-mandt.
              wa_zmmt0123-werks           = wa_zmmt0122-werks.
              wa_zmmt0123-lgort           = wa_zmmt0122-lgort.
              wa_zmmt0123-nfenum          = wa_j_1bnfe_active-nfnum9.
              wa_zmmt0123-dt_atualizacao  = sy-datum.
              wa_zmmt0123-type            = wa_return-type.
              wa_zmmt0123-id              = wa_return-id.
              wa_zmmt0123-message         = wa_return-message.

*---> CS1005885 --->
              DELETE FROM zmmt0123 WHERE  nfenum EQ wa_j_1bnfe_active-nfnum9.

              INSERT zmmt0123 FROM wa_zmmt0123.

              COMMIT WORK.

*              MODIFY  zmmt0123 FROM wa_zmmt0123.
*---> CS1005885 --->
              CLEAR: wa_zmmt0123, wa_return.
              p_erro = 'X'.
            ENDLOOP.
          ENDIF.

          IF p_erro IS INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            xdocmat315 =  wl_mblnr.
          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
              IMPORTING
                return = tl_return2.
            CLEAR: wl_header,wl_code,wl_mblnr,p_erro.
            FREE: tl_item,tl_return.
            CONTINUE.
          ENDIF.
        ENDIF.

        CLEAR: wl_header, wl_code, tl_item, wl_mjahr, wl_mblnr, p_erro.
        FREE: tl_return[], tl_item[].

        IF wa_zsdt0001-doc_material EQ ' ' AND wa_zmmt0122-tp_mov_mat NE ' '.

          wl_header-pstng_date  =  wa_zsdt0001-dt_movimento.
          wl_header-doc_date    =  wa_zsdt0001-dt_movimento.
          wl_header-header_txt  =  wa_j_1bnfe_active-nfnum9.

          wl_code-gm_code       = '06'.


*---> 06/07/2023 - Migração S4 - MA
*        tl_item-material      =  wa_zfiwrt0009-matnr.

          CLEAR v_len.

          v_len = strlen( wa_zfiwrt0009-matnr ).

          IF v_len > 18.
            tl_item-material_long =  wa_zfiwrt0009-matnr.

          ELSE.
            tl_item-material      =  wa_zfiwrt0009-matnr.
          ENDIF.
*<--- 06/07/2023 - Migração S4 - MA

          tl_item-plant         =  wa_zsdt0001-branch.
          tl_item-stge_loc      =  wa_zfiwrt0008-move_stloc.
          tl_item-batch         =  wa_zfiwrt0009-charg.
          tl_item-move_type     =  wa_zmmt0122-tp_mov_mat.
* Início - Sara - CS2020000884  - Agosto/2020
* PBI - 70320 - inicio - cbrand
          tl_item-entry_qnt     =  wa_zfiwrt0009-menge.
*          tl_item-entry_qnt     =  wa_zsdt0001-peso_liq.
* PBI - 70320 - Fim - cbrand
* Fim - Sara - CS2020000884  - Agosto/2020


*---> 06/07/2023 - Migração S4 - MA
*        tl_item-move_mat      =  _normt.
          CLEAR v_len.

          v_len = strlen( _normt ).

          IF v_len > 18.
            tl_item-move_mat_long =  _normt.
          ELSE.
            tl_item-move_mat       =  _normt.
          ENDIF.
*<--- 06/07/2023 - Migração S4 - MA

          tl_item-move_plant    =  wa_zsdt0001-branch.
          tl_item-move_stloc    =  wa_zfiwrt0008-move_stloc.

          APPEND tl_item.


          FREE: tl_return.
          CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              goodsmvt_header  = wl_header
              goodsmvt_code    = wl_code
            IMPORTING
              materialdocument = wl_mblnr
              matdocumentyear  = wl_mjahr
            TABLES
              goodsmvt_item    = tl_item
              return           = tl_return.

          IF sy-subrc IS INITIAL.

            LOOP AT tl_return  INTO wa_return WHERE type EQ 'E' .

              wa_zmmt0123-mandt           = sy-mandt.
              wa_zmmt0123-werks           = wa_zmmt0122-werks.
              wa_zmmt0123-lgort           = wa_zmmt0122-lgort.
              wa_zmmt0123-nfenum          = wa_j_1bnfe_active-nfnum9.
              wa_zmmt0123-dt_atualizacao  = sy-datum.
              wa_zmmt0123-type            = wa_return-type.
              wa_zmmt0123-id              = wa_return-id.
              wa_zmmt0123-message         = wa_return-message.

*---> CS1005885 --->
              DELETE FROM zmmt0123 WHERE  nfenum EQ wa_j_1bnfe_active-nfnum9.

              INSERT zmmt0123 FROM wa_zmmt0123.

              COMMIT WORK.

*              MODIFY  zmmt0123 FROM wa_zmmt0123.
*---> CS1005885 --->

              CLEAR: wa_zmmt0123, wa_return.

              p_erro = 'X'.
            ENDLOOP.

            IF p_erro IS INITIAL.
              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              xdocmat309 = wl_mblnr.
            ELSE.
              IF xdocmat315 IS NOT INITIAL.
                "Estona movimento 315.
                FREE: tl_return[].
                CLEAR: wl_headret.
                CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
                  EXPORTING
                    materialdocument = xdocmat315
                    matdocumentyear  = wl_mjahr
                  IMPORTING
                    goodsmvt_headret = wl_headret
                  TABLES
                    return           = tl_return[].

                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                  EXPORTING
                    wait = 'X'.
              ENDIF.
              CONTINUE.
            ENDIF.
          ELSE.

            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
              IMPORTING
                return = tl_return2.

            IF xdocmat315 IS NOT INITIAL.
              "Estona movimento 315.
              FREE: tl_return[].
              CLEAR: wl_headret.
              CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
                EXPORTING
                  materialdocument = xdocmat315
                  matdocumentyear  = wl_mjahr
                IMPORTING
                  goodsmvt_headret = wl_headret
                TABLES
                  return           = tl_return[].

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
            ENDIF.
            CONTINUE.
          ENDIF.
        ENDIF.

* Início - Sara - CS2020000884  - Agosto/2020
        "Criar Docto para Movimento Quebra
        CLEAR: wl_header, wl_code, tl_item, wl_mjahr, wl_mblnr, xdocmat315_q, lv_peso_quebra, p_erro.
        FREE: tl_return[], tl_item[].

        IF wa_zsdt0001-doc_material_e EQ ' ' AND wa_zmmt0122-tp_mov_qb NE ' '.

          lv_peso_quebra      = ( wa_zsdt0001-peso_liq - wa_zfiwrt0009-menge ).
          IF lv_peso_quebra < 0.
            lv_peso_quebra = lv_peso_quebra * -1.
          ENDIF.
* pbi - 70230 - inicio - cbrand
          lv_peso_quebra = 0.
* pbi - 70230 - fim - cbrand
          IF lv_peso_quebra IS NOT INITIAL.

            wl_header-pstng_date  = wa_zsdt0001-dt_movimento.
            wl_header-doc_date    = wa_zsdt0001-dt_movimento.
            wl_header-header_txt  = wa_j_1bnfe_active-nfnum9.

            wl_code-gm_code       = '06'.


*---> 06/07/2023 - Migração S4 - MA
*        tl_item-material      = wa_zfiwrt0009-matnr.
            CLEAR v_len.

            v_len = strlen( wa_zfiwrt0009-matnr ).

            IF v_len > 18.
              tl_item-material_long = wa_zfiwrt0009-matnr.
            ELSE.
              tl_item-material      = wa_zfiwrt0009-matnr.
            ENDIF.
*<--- 06/07/2023 - Migração S4 - MA
            tl_item-plant	        =	wa_zsdt0001-branch.
            tl_item-stge_loc      = wa_zfiwrt0008-move_stloc.
            tl_item-batch         = wa_zfiwrt0009-charg.
            tl_item-move_type     = wa_zmmt0122-tp_mov_qb.
            tl_item-entry_qnt     = lv_peso_quebra.

            APPEND tl_item.

            CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
              EXPORTING
                goodsmvt_header  = wl_header
                goodsmvt_code    = wl_code
              IMPORTING
                materialdocument = wl_mblnr
                matdocumentyear  = wl_mjahr
              TABLES
                goodsmvt_item    = tl_item
                return           = tl_return.

            IF sy-subrc IS INITIAL.

              LOOP AT tl_return  INTO wa_return  WHERE type EQ 'E' .
                wa_zmmt0123-mandt           = sy-mandt.
                wa_zmmt0123-werks           = wa_zmmt0122-werks.
                wa_zmmt0123-lgort           = wa_zmmt0122-lgort.
                wa_zmmt0123-nfenum          = wa_j_1bnfe_active-nfnum9.
                wa_zmmt0123-dt_atualizacao  = sy-datum.
                wa_zmmt0123-type            = wa_return-type.
                wa_zmmt0123-id              = wa_return-id.
                wa_zmmt0123-message         = wa_return-message.

*---> CS1005885 --->
                DELETE FROM zmmt0123 WHERE  nfenum EQ wa_j_1bnfe_active-nfnum9.

                INSERT zmmt0123 FROM wa_zmmt0123.

                COMMIT WORK.

*              MODIFY  zmmt0123 FROM wa_zmmt0123.
*---> CS1005885 --->

                CLEAR: wa_zmmt0123, wa_return.
              ENDLOOP.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              xdocmat315_q =  wl_mblnr.


            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
                IMPORTING
                  return = tl_return2.
            ENDIF.
          ENDIF.

        ENDIF.

* Fim - Sara - CS2020000884  - Agosto/2020


        IF xdocmat309 IS NOT INITIAL AND   xdocmat315 IS NOT INITIAL.

          _xdocmat309 =   xdocmat309.
          _xdocmat315 =   xdocmat315.

          wa_zsdt0001-doc_material     =  xdocmat309.
          wa_zsdt0001-ano_material     =  sy-datum+0(4).
          wa_zsdt0001-doc_material_e   =  xdocmat315.
          wa_zsdt0001-ano_material_e   =  sy-datum+0(4).

          MODIFY zsdt0001 FROM wa_zsdt0001.
          COMMIT WORK.

          wa_zfiwrt0008-doc_material_e = xdocmat315.

* Início - Sara - CS2020000884  - Agosto/2020
          CLEAR: wa_zfiwrt0008-doc_material_q, wa_zfiwrt0009-peso_quebra, wa_zsdt0001.
          IF xdocmat315_q IS NOT INITIAL.
            _xdocmat315_q = xdocmat315_q.
            wa_zfiwrt0008-doc_material_q = xdocmat315_q.
            wa_zfiwrt0009-peso_quebra    = lv_peso_quebra.

            MODIFY zfiwrt0009 FROM wa_zfiwrt0009.
            COMMIT WORK.

          ENDIF.
* Fim - Sara - CS2020000884  - Agosto/2020
          MODIFY zfiwrt0008 FROM wa_zfiwrt0008.
          COMMIT WORK.
          CLEAR: wa_zfiwrt0008.
        ENDIF.


        CLEAR: wa_zfiwrt0004,  wa_zfiwrt0008,  wa_zfiwrt0009, wa_zsdt0001, wl_header, wl_code, tl_item, wl_mjahr, wl_mblnr, p_erro.
        CLEAR: xregio,  xnfyear,  xnfmonth,  xstcd1,  xmodel,  xserie,  xnfnum9,  xdocnum9,  xcdv,  xdocmat315,  xdocmat309, dt_mov_ent.

* Início - Sara - CS2020000884  - Agosto/2020
        CLEAR: xdocmat315_q, lv_peso_quebra.
* Fim - Sara - CS2020000884  - Agosto/2020

        FREE: tl_return[], tl_item[].
      ENDLOOP.

      CLEAR:   wa_zmmt0122, _parid.
    ENDLOOP.

  ELSE. " +++ETAPA MANUAL

    CLEAR: wa_t001k.
    SELECT SINGLE * FROM t001k INTO wa_t001k
      WHERE bwkey EQ p_werks.

    CLEAR: wa_zmmt0122.
    SELECT SINGLE *  FROM zmmt0122 INTO wa_zmmt0122
     WHERE werks EQ p_werks.

    _parid = |{ wa_t001k-bwkey ALPHA = IN }|.

    FREE: it_zsdt0001.
    SELECT * FROM zsdt0001 INTO TABLE it_zsdt0001
      WHERE dt_movimento    EQ p_dt_movimento
      AND   tp_movimento    EQ 'E'
      AND   bukrs           EQ wa_t001k-bukrs
      AND   branch          EQ wa_t001k-bwkey
      AND   parid           EQ _parid
      AND  (  doc_material  EQ ' '  OR  doc_material_e EQ ' ' ).

    LOOP AT it_zsdt0001 INTO wa_zsdt0001.

      xregio   = wa_zsdt0001-chave_nfe+0(2).
      xnfyear  = wa_zsdt0001-chave_nfe+2(2).
      xnfmonth = wa_zsdt0001-chave_nfe+4(2).
      xstcd1   = wa_zsdt0001-chave_nfe+6(14).
      xmodel   = wa_zsdt0001-chave_nfe+20(2).
      xserie   = wa_zsdt0001-chave_nfe+22(3).
      xnfnum9  = wa_zsdt0001-chave_nfe+25(9).
      xdocnum9 = wa_zsdt0001-chave_nfe+34(9).
      xcdv     = wa_zsdt0001-chave_nfe+43(1).

      CLEAR: wa_j_1bnfe_active.
      SELECT SINGLE * FROM j_1bnfe_active INTO wa_j_1bnfe_active
        WHERE docsta  EQ 1
        AND   regio   EQ xregio
        AND   nfyear  EQ xnfyear
        AND   nfmonth EQ xnfmonth
        AND   stcd1   EQ xstcd1
        AND   model   EQ xmodel
        AND   serie   EQ xserie
        AND   nfnum9  EQ xnfnum9
        AND   docnum9 EQ xdocnum9
        AND   cdv     EQ xcdv
*-CS1105820-#RIMINI-21.06.2023-BEGIN
        AND   cancel  EQ space.
*-CS1105820-#RIMINI-21.06.2023-END
      IF sy-subrc EQ 0.

        CLEAR: wa_j_1bnflin.
        SELECT SINGLE * FROM j_1bnflin INTO wa_j_1bnflin
          WHERE docnum EQ wa_j_1bnfe_active-docnum.

        IF sy-subrc = 0.
          CLEAR: wa_zfiwrt0008.
          SELECT SINGLE * FROM zfiwrt0008 INTO wa_zfiwrt0008  WHERE seq_lcto  EQ wa_j_1bnflin-refkey.

          IF sy-subrc = 0.

            CLEAR: wa_zfiwrt0004.
            SELECT SINGLE * FROM  zfiwrt0004 INTO wa_zfiwrt0004  WHERE operacao EQ  wa_zfiwrt0008-operacao.

            IF wa_zfiwrt0004-bwart NE wa_zmmt0122-tp_mov_sd.
*--> CS1002304 --->
              wa_zmmt0123-mandt           = sy-mandt.
              wa_zmmt0123-werks           = wa_zmmt0122-werks.
              wa_zmmt0123-lgort           = wa_zmmt0122-lgort.
              wa_zmmt0123-nfenum          = wa_j_1bnfe_active-nfnum9.
              wa_zmmt0123-dt_atualizacao  = sy-datum.
              wa_zmmt0123-type            = 'E'.
              wa_zmmt0123-id              = 'tabela zfiwrt0004'.
              wa_zmmt0123-message         = 'NFe:Verifique cod.operacao e tipo movimento-zfiwrt0004'.
              MODIFY  zmmt0123 FROM wa_zmmt0123.
              CLEAR: wa_zmmt0123, wa_return.
*--> CS1002304 --->
              p_erro = 'X'.
              CONTINUE.
            ENDIF.

          ENDIF.

          CLEAR: wa_zfiwrt0009.
          SELECT SINGLE * FROM zfiwrt0009 INTO wa_zfiwrt0009  WHERE seq_lcto  EQ  wa_j_1bnflin-refkey.

          IF sy-subrc = 0.

            "Converte material 18 posições.
            CLEAR: vg_matnr.
            wa_zfiwrt0009-matnr = |{ wa_zfiwrt0009-matnr ALPHA = OUT }|.
            vg_matnr = wa_zfiwrt0009-matnr.
            vg_matnr = |{ vg_matnr ALPHA = IN }|.
            CLEAR: wa_zfiwrt0009-matnr.
            wa_zfiwrt0009-matnr = vg_matnr.

            CLEAR: wa_mara.
            SELECT SINGLE * FROM mara INTO wa_mara  WHERE matnr EQ  wa_zfiwrt0009-matnr.

            IF wa_mara-normt IS INITIAL.
*--> CS1002304 --->
              wa_zmmt0123-mandt           = sy-mandt.
              wa_zmmt0123-werks           = wa_zmmt0122-werks.
              wa_zmmt0123-lgort           = wa_zmmt0122-lgort.
              wa_zmmt0123-nfenum          = wa_j_1bnfe_active-nfnum9.
              wa_zmmt0123-dt_atualizacao  = sy-datum.
              wa_zmmt0123-type            = 'E'.
              wa_zmmt0123-id              = ''.
              wa_zmmt0123-message         = 'Denominação da norma (p.ex. DIN)'.
*---> CS1005885 --->
              DELETE FROM zmmt0123 WHERE  nfenum EQ wa_j_1bnfe_active-nfnum9.

              INSERT zmmt0123 FROM wa_zmmt0123.

              COMMIT WORK.

*              MODIFY  zmmt0123 FROM wa_zmmt0123.
*---> CS1005885 --->
              CLEAR: wa_zmmt0123, wa_return.
              p_erro = 'X'.
*--> CS1002304 --->
              CONTINUE.
            ELSE.

              _normt = |{ wa_mara-normt ALPHA = IN }|.

              "Converte material 18 posições.
              CLEAR: vg_matnr.
              _normt = |{ _normt ALPHA = OUT }|.
              vg_matnr = _normt.
              vg_matnr = |{ vg_matnr ALPHA = IN }|.
              CLEAR: _normt.
              _normt = vg_matnr.

              CLEAR: wa_mara2.
              SELECT SINGLE * FROM mara INTO wa_mara2  WHERE matnr EQ _normt.

              IF wa_mara2-mtart NE 'ZROH' AND wa_mara2-mtart NE 'ZHAW'..
*--> CS1002304 --->
                wa_zmmt0123-mandt           = sy-mandt.
                wa_zmmt0123-werks           = wa_zmmt0122-werks.
                wa_zmmt0123-lgort           = wa_zmmt0122-lgort.
                wa_zmmt0123-nfenum          = wa_j_1bnfe_active-nfnum9.
                wa_zmmt0123-dt_atualizacao  = sy-datum.
                wa_zmmt0123-type            = 'E'.
                wa_zmmt0123-id              = ''.
                wa_zmmt0123-message         = 'Verifique Tipo de material'.
*---> CS1005885 --->
                DELETE FROM zmmt0123 WHERE  nfenum EQ wa_j_1bnfe_active-nfnum9.

                INSERT zmmt0123 FROM wa_zmmt0123.

                COMMIT WORK.

*              MODIFY  zmmt0123 FROM wa_zmmt0123.
*---> CS1005885 --->
                CLEAR: wa_zmmt0123, wa_return.
                p_erro = 'X'.
*--> CS1002304 --->
                CONTINUE.
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF wa_zsdt0001-doc_material_e EQ ' ' AND wa_zmmt0122-tp_mov_et NE ' '.

        wl_header-pstng_date  = wa_zsdt0001-dt_movimento.
        wl_header-doc_date    = wa_zsdt0001-dt_movimento.
        wl_header-header_txt  = wa_j_1bnfe_active-nfnum9.

        wl_code-gm_code       = '06'.


*---> 06/07/2023 - Migração S4 - MA
*        tl_item-material      = wa_zfiwrt0009-matnr.

        CLEAR v_len.

        v_len = strlen( wa_zfiwrt0009-matnr ).

        IF v_len > 18.
          tl_item-material_long = wa_zfiwrt0009-matnr.

        ELSE.
          tl_item-material      = wa_zfiwrt0009-matnr.
        ENDIF.
*<--- 06/07/2023 - Migração S4 - MA

        tl_item-plant	        =	wa_zsdt0001-branch.
        tl_item-stge_loc      = wa_zfiwrt0008-move_stloc.
        tl_item-batch         = wa_zfiwrt0009-charg.
        tl_item-move_type     = wa_zmmt0122-tp_mov_et.
* Início - Sara - CS2020000884  - Agosto/2020
        tl_item-entry_qnt     = wa_zfiwrt0009-menge.
*        tl_item-entry_qnt     = wa_zsdt0001-peso_liq.
* Fim - Sara - CS2020000884  - Agosto/2020
        APPEND tl_item.

        CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            goodsmvt_header  = wl_header
            goodsmvt_code    = wl_code
          IMPORTING
            materialdocument = wl_mblnr
            matdocumentyear  = wl_mjahr
          TABLES
            goodsmvt_item    = tl_item
            return           = tl_return.

        IF sy-subrc IS INITIAL.

          LOOP AT tl_return  INTO wa_return  WHERE type EQ 'E' .
            wa_zmmt0123-mandt           = sy-mandt.
            wa_zmmt0123-werks           = wa_zmmt0122-werks.
            wa_zmmt0123-lgort           = wa_zmmt0122-lgort.
            wa_zmmt0123-nfenum          = wa_j_1bnfe_active-nfnum9.
            wa_zmmt0123-dt_atualizacao  = sy-datum.
            wa_zmmt0123-type            = wa_return-type.
            wa_zmmt0123-id              = wa_return-id.
            wa_zmmt0123-message         = wa_return-message.

*---> CS1005885 --->
            DELETE FROM zmmt0123 WHERE  nfenum EQ wa_j_1bnfe_active-nfnum9.

            INSERT zmmt0123 FROM wa_zmmt0123.

            COMMIT WORK.

*              MODIFY  zmmt0123 FROM wa_zmmt0123.
*---> CS1005885 --->
            " INSERT   zmmt0123 FROM wa_zmmt0123.
            CLEAR: wa_zmmt0123, wa_return.

            p_erro = 'X'.
          ENDLOOP.
        ENDIF.

        IF p_erro IS INITIAL.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.

          xdocmat315 =  wl_mblnr.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
            IMPORTING
              return = tl_return2.
          CLEAR: wl_header,wl_code,wl_mblnr,p_erro.
          FREE: tl_item,tl_return.
          CONTINUE.
        ENDIF.
      ENDIF.

*          IF p_erro IS INITIAL.
*            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*              EXPORTING
*                wait = 'X'.
*
*            xdocmat315 =  wl_mblnr.
*          ELSE.
*            CONTINUE.
*          ENDIF.
*
*        ELSE.
*          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
*            IMPORTING
*              return = tl_return2.
*          CONTINUE.
*        ENDIF.
*      ENDIF.

      CLEAR: wl_header, wl_code, tl_item, wl_mjahr, wl_mblnr, p_erro.
      FREE: tl_return[], tl_item[].

      IF wa_zsdt0001-doc_material EQ ' ' AND wa_zmmt0122-tp_mov_mat NE ' '.

        wl_header-pstng_date  =  wa_zsdt0001-dt_movimento.
        wl_header-doc_date    =  wa_zsdt0001-dt_movimento.
        wl_header-header_txt  =  wa_j_1bnfe_active-nfnum9.

        wl_code-gm_code       = '06'.

*---> 06/07/2023 - Migração S4 - MA
*        tl_item-material      =  wa_zfiwrt0009-matnr.
        CLEAR v_len.

        v_len = strlen( wa_zfiwrt0009-matnr ).

        IF v_len > 18.
          tl_item-material_long = wa_zfiwrt0009-matnr.

        ELSE.
          tl_item-material      = wa_zfiwrt0009-matnr.
        ENDIF.
*<--- 06/07/2023 - Migração S4 - MA

        tl_item-plant         =  wa_zsdt0001-branch.
        tl_item-stge_loc      =  wa_zfiwrt0008-move_stloc.
        tl_item-batch         =  wa_zfiwrt0009-charg.
        tl_item-move_type     =  wa_zmmt0122-tp_mov_mat.
* Início - Sara - CS2020000884  - Agosto/2020
* pbi - 70320 - inicio - cbrand
        tl_item-entry_qnt     =  wa_zfiwrt0009-menge.
*        tl_item-entry_qnt     = wa_zsdt0001-peso_liq.
* pbi - 70320 - fim - cbrand
* Fim - Sara - CS2020000884  - Agosto/2020


*---> 06/07/2023 - Migração S4 - MA
*        tl_item-move_mat      =  _normt.
        CLEAR v_len.
        v_len = strlen( _normt ).

        IF v_len > 18.
          tl_item-move_mat_long = _normt.
        ELSE.
          tl_item-move_mat      = _normt.
        ENDIF.
*<--- 06/07/2023 - Migração S4 - MA

        tl_item-move_plant    =  wa_zsdt0001-branch.
        tl_item-move_stloc    =  wa_zfiwrt0008-move_stloc.

        APPEND tl_item.


        CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            goodsmvt_header  = wl_header
            goodsmvt_code    = wl_code
          IMPORTING
            materialdocument = wl_mblnr
            matdocumentyear  = wl_mjahr
          TABLES
            goodsmvt_item    = tl_item
            return           = tl_return.

        IF sy-subrc IS INITIAL.

          LOOP AT tl_return  INTO wa_return WHERE type EQ 'E' .

            wa_zmmt0123-mandt           = sy-mandt.
            wa_zmmt0123-werks           = wa_zmmt0122-werks.
            wa_zmmt0123-lgort           = wa_zmmt0122-lgort.
            wa_zmmt0123-nfenum          = wa_j_1bnfe_active-nfnum9.
            wa_zmmt0123-dt_atualizacao  = sy-datum.
            wa_zmmt0123-type            = wa_return-type.
            wa_zmmt0123-id              = wa_return-id.
            wa_zmmt0123-message         = wa_return-message.

*---> CS1005885 --->
            DELETE FROM zmmt0123 WHERE  nfenum EQ wa_j_1bnfe_active-nfnum9.

            INSERT zmmt0123 FROM wa_zmmt0123.

            COMMIT WORK.

*              MODIFY  zmmt0123 FROM wa_zmmt0123.
*---> CS1005885 --->
            CLEAR: wa_zmmt0123, wa_return.

            p_erro = 'X'.
          ENDLOOP.

          IF p_erro IS INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            xdocmat309 = wl_mblnr.
          ELSE.
            IF xdocmat315 IS NOT INITIAL.
              "Estona movimento 315 se houver erro ao fazer ao movimento 309.
              FREE: tl_return[].
              CLEAR: wl_headret.
              CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
                EXPORTING
                  materialdocument = xdocmat315
                  matdocumentyear  = wl_mjahr
                IMPORTING
                  goodsmvt_headret = wl_headret
                TABLES
                  return           = tl_return[].

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.
            ENDIF.
            CONTINUE.
          ENDIF.
        ELSE.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
            IMPORTING
              return = tl_return2.

          IF xdocmat315 IS NOT INITIAL.
            "Estona movimento 315 se houver erro ao fazer ao movimento 309.
            FREE: tl_return[].
            CLEAR: wl_headret.
            CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
              EXPORTING
                materialdocument = xdocmat315
                matdocumentyear  = wl_mjahr
              IMPORTING
                goodsmvt_headret = wl_headret
              TABLES
                return           = tl_return[].

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

          ENDIF.
          CONTINUE.
        ENDIF.
      ENDIF.

* Início - Sara - CS2020000884  - Agosto/2020
      "Criar Docto para Movimento Quebra
      CLEAR: wl_header, wl_code, tl_item, wl_mjahr, wl_mblnr, xdocmat315_q, lv_peso_quebra, p_erro.
      REFRESH: tl_return[], tl_item[].

      IF wa_zsdt0001-doc_material_e EQ ' ' AND wa_zmmt0122-tp_mov_qb NE ' '.

        lv_peso_quebra = ( wa_zsdt0001-peso_liq - wa_zfiwrt0009-menge ).
        IF lv_peso_quebra < 0.
          lv_peso_quebra = lv_peso_quebra * -1.
        ENDIF.
* pbi - 70320 - inicio - cbrand
        lv_peso_quebra = 0.
* pbi - 70320 - fim - cbrand
        IF lv_peso_quebra IS NOT INITIAL.

          wl_header-pstng_date  = wa_zsdt0001-dt_movimento.
          wl_header-doc_date    = wa_zsdt0001-dt_movimento.
          wl_header-header_txt  = wa_j_1bnfe_active-nfnum9.

          wl_code-gm_code       = '06'.


*---> 06/07/2023 - Migração S4 - MA
*        tl_item-material      = wa_zfiwrt0009-matnr.
          CLEAR v_len.

          v_len = strlen( wa_zfiwrt0009-matnr ).

          IF v_len > 18.
            tl_item-material_long =  wa_zfiwrt0009-matnr.

          ELSE.
            tl_item-material      =  wa_zfiwrt0009-matnr.
          ENDIF.
*<--- 06/07/2023 - Migração S4 - MA

          tl_item-plant	        =	wa_zsdt0001-branch.
          tl_item-stge_loc      = wa_zfiwrt0008-move_stloc.
          tl_item-batch         = wa_zfiwrt0009-charg.
          tl_item-move_type     = wa_zmmt0122-tp_mov_qb.
          tl_item-entry_qnt     = lv_peso_quebra.

          APPEND tl_item.

          CALL FUNCTION 'BAPI_GOODSMVT_CREATE' "#EC CI_USAGE_OK[2438131]
            EXPORTING
              goodsmvt_header  = wl_header
              goodsmvt_code    = wl_code
            IMPORTING
              materialdocument = wl_mblnr
              matdocumentyear  = wl_mjahr
            TABLES
              goodsmvt_item    = tl_item
              return           = tl_return.

          IF sy-subrc IS INITIAL.

            LOOP AT tl_return  INTO wa_return  WHERE type EQ 'E' .
              wa_zmmt0123-mandt           = sy-mandt.
              wa_zmmt0123-werks           = wa_zmmt0122-werks.
              wa_zmmt0123-lgort           = wa_zmmt0122-lgort.
              wa_zmmt0123-nfenum          = wa_j_1bnfe_active-nfnum9.
              wa_zmmt0123-dt_atualizacao  = sy-datum.
              wa_zmmt0123-type            = wa_return-type.
              wa_zmmt0123-id              = wa_return-id.
              wa_zmmt0123-message         = wa_return-message.

*---> CS1005885 --->
              DELETE FROM zmmt0123 WHERE  nfenum EQ wa_j_1bnfe_active-nfnum9.

              INSERT zmmt0123 FROM wa_zmmt0123.

              COMMIT WORK.

*              MODIFY  zmmt0123 FROM wa_zmmt0123.
*---> CS1005885 --->
              CLEAR: wa_zmmt0123, wa_return.
            ENDLOOP.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            xdocmat315_q =  wl_mblnr.

          ELSE.
            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
              IMPORTING
                return = tl_return2.
          ENDIF.

        ENDIF.

      ENDIF.

* Fim - Sara - CS2020000884  - Agosto/2020

      IF xdocmat309 IS NOT INITIAL AND   xdocmat315 IS NOT INITIAL.
        _xdocmat309 =   xdocmat309.
        _xdocmat315 =   xdocmat315.

        wa_zsdt0001-doc_material     =  xdocmat309.
        wa_zsdt0001-ano_material     =  sy-datum+0(4).
        wa_zsdt0001-doc_material_e   =  xdocmat315.
        wa_zsdt0001-ano_material_e   =  sy-datum+0(4).
        MODIFY zsdt0001 FROM wa_zsdt0001.
        COMMIT WORK.

        wa_zfiwrt0008-doc_material_trm = xdocmat309.
        wa_zfiwrt0008-doc_material_e   = xdocmat315.

* Início - Sara - CS2020000884  - Agosto/2020
        CLEAR: wa_zfiwrt0008-doc_material_q, wa_zfiwrt0009-peso_quebra.
        IF xdocmat315_q IS NOT INITIAL.
          _xdocmat315_q = xdocmat315_q.
          wa_zfiwrt0008-doc_material_q = xdocmat315_q.
          wa_zfiwrt0009-peso_quebra    = lv_peso_quebra.

          MODIFY zfiwrt0009 FROM wa_zfiwrt0009.
          COMMIT WORK.

        ENDIF.
* Fim - Sara - CS2020000884  - Agosto/2020

        MODIFY zfiwrt0008 FROM wa_zfiwrt0008.
        COMMIT WORK.
      ENDIF.

      CLEAR: wa_zfiwrt0004,  wa_zfiwrt0008,  wa_zfiwrt0009, wa_zsdt0001, wl_header, wl_code, tl_item, wl_mjahr, wl_mblnr, _parid,  _normt.
      CLEAR: xregio,  xnfyear,  xnfmonth,  xstcd1,  xmodel,  xserie,  xnfnum9,  xdocnum9,  xcdv,  xdocmat315,  xdocmat309, dt_mov_ent, _normt.

* Início - Sara - CS2020000884  - Agosto/2020
      CLEAR: xdocmat315_q, lv_peso_quebra.
* Fim - Sara - CS2020000884  - Agosto/2020

      FREE: tl_return[], tl_item[].
    ENDLOOP.

  ENDIF.

ENDFUNCTION.
