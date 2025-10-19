  FUNCTION zsdf_busca_saldo_rfl.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_DOCNUM_PROD) TYPE  ZSDT_ITMNUM
*"     REFERENCE(I_COD_RA_EMBARQUE) TYPE  ZDE_CODIGO_RA_EMBARQUE
*"     REFERENCE(I_EUDR) TYPE  ZEUDR
*"     REFERENCE(I_RETORNAR_EUDR) TYPE  C DEFAULT SPACE
*"     REFERENCE(I_TP_VINC1) TYPE  C OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_RFL) TYPE  ZSDT_RFL
*"     REFERENCE(E_RFL_SALDO_FISICO) TYPE  ZSDT_RFL_SALDO_FISICO
*"----------------------------------------------------------------------
*-----------------------------------------------------------------------
*& Histórico de Alterações:                                            *
*-----------------------------------------------------------------------
*&  Data     |Request    | Autor       | Alteração                     *
*&----------------------------------------------------------------------
*& 30/01/2025|DEVK9A1XAW |NSEGATIN     | Define a Nota Fiscal de Saída *
*&                                     |conforme RA e o EUDR (163355). *
*-----------------------------------------------------------------------

    TYPES:
      BEGIN OF ty_rem_notas,
        id_nomeacao_tran TYPE znom_reme_notas-id_nomeacao_tran,
        id_empresa       TYPE znom_reme_notas-id_empresa,
        id_filial        TYPE znom_reme_notas-id_filial,
        id_material      TYPE znom_reme_notas-id_material,
        id_remetente     TYPE znom_reme_notas-id_remetente,
        docnum           TYPE znom_reme_notas-docnum,
        itmnum           TYPE znom_reme_notas-itmnum,
        grp_retorno      TYPE znom_reme_notas-grp_retorno,
        nr_quantidade    TYPE znom_reme_notas-nr_quantidade,
      END OF ty_rem_notas,

      BEGIN OF ty_rem_notas_sum,
        docnum        TYPE znom_reme_notas-docnum,
        nr_quantidade TYPE znom_reme_notas-nr_quantidade,
      END OF ty_rem_notas_sum.

    TYPES:
      BEGIN OF ty_j_1bnflin,
        docnum TYPE j_1bnflin-docnum,
        docdat TYPE j_1bnfdoc-docdat,
        itmnum TYPE j_1bnflin-itmnum,
        werks  TYPE j_1bnflin-werks,
        cfop   TYPE j_1bnflin-cfop,
        menge  TYPE j_1bnflin-menge,
      END OF ty_j_1bnflin,

      BEGIN OF ty_retlote_sum,
        docnum TYPE zsdt_retlote-docnum,
        qtd    TYPE zsdt_retlote-quant_vinc,
      END OF ty_retlote_sum,

      BEGIN OF ty_rem_saldo,
        docnum TYPE j_1bnflin-docnum,
        saldo  TYPE menge_d,
      END OF ty_rem_saldo.

    DATA: lt_j_1bnflin          TYPE TABLE OF ty_j_1bnflin,
          lt_retlote_sum        TYPE TABLE OF ty_retlote_sum,
          ls_retlote_sum        TYPE ty_retlote_sum,
          lt_rem_saldo          TYPE TABLE OF ty_rem_saldo,
          ls_rem_saldo          TYPE ty_rem_saldo,
          lv_qtd                TYPE zsdt_retlote-quant_vinc,
          lv_saldo              TYPE zsdt_retlote-quant_vinc,
          lv_saldo_fisico       TYPE zsdt_retlote-quant_vinc,
          lt_rem_notas          TYPE TABLE OF ty_rem_notas,
          lt_rem_notas_sum      TYPE TABLE OF ty_rem_notas_sum,
          ls_rem_notas_sum      TYPE ty_rem_notas_sum,
          ls_alv_vinc_lotes     TYPE zsd_rfl,
          ls_alv_vinc_lotes_aux TYPE zsd_rfl,
          lv_docnum_eprod_ant   TYPE j_1bnfdoc-docnum,
          lv_qtd_rem            TYPE zsdt_retlote-quant_vinc,
          lv_qtd_rem2           TYPE zsdt_retlote-quant_vinc,
          lv_pular              TYPE c,
          lv_sair               TYPE c,
          lt_saldo_eprod        TYPE TABLE OF ty_rem_saldo.

    DATA(lt_notas_fiscais_alv) = i_docnum_prod.
    SORT lt_notas_fiscais_alv BY docnum.
    DELETE ADJACENT DUPLICATES FROM lt_notas_fiscais_alv COMPARING docnum.

*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Início de Alteração
    v_tp_vinc1        = i_tp_vinc1.
    t_docnum_prod_rfl = i_docnum_prod.

    DELETE t_docnum_prod_rfl WHERE nf_chegada_porto NE 'RFL'.
    SORT t_docnum_prod_rfl BY docnum.
*** Stefanini - IR245313 - 27/06/2025 - LAZAROSR - Fim de Alteração

    SELECT *
      FROM zsdtvinc_p_flote
      INTO TABLE @DATA(lt_flote)
      FOR ALL ENTRIES IN @lt_notas_fiscais_alv
      WHERE docnum_eprod EQ @lt_notas_fiscais_alv-docnum
        AND cancel       EQ @abap_false.
    IF sy-subrc IS INITIAL.
      SORT lt_flote BY docnum_flote docnum_eprod ASCENDING.
**<<<------"163355 - NMS - INI------>>>
* Define a Nota Fiscal de Saída conforme RA e o EUDR.
      PERFORM zf_define_nota_saida TABLES lt_flote
                                    USING i_cod_ra_embarque
                                          i_eudr
                                          i_retornar_eudr
                                          sy-abcde+18(1). "S - Saída
**<<<------"163355 - NMS - FIM------>>>
      DATA(lt_flote_aux) = lt_flote.
      SORT lt_flote_aux BY docnum_flote.
      DELETE ADJACENT DUPLICATES FROM lt_flote_aux COMPARING docnum_flote.

      SELECT *
        FROM zsdtvinc_p_flote
        INTO TABLE @DATA(lt_flote2)
        FOR ALL ENTRIES IN @lt_flote_aux
        WHERE docnum_flote EQ @lt_flote_aux-docnum_flote
          AND cancel       EQ @abap_false.
      IF sy-subrc IS INITIAL.
        SORT lt_flote2 BY docnum_flote docnum_eprod ASCENDING.
**<<<------"163355 - NMS - INI------>>>
* Define a Nota Fiscal de Saída conforme RA e o EUDR.
        PERFORM zf_define_nota_saida TABLES lt_flote2
                                      USING i_cod_ra_embarque
                                            i_eudr
                                            i_retornar_eudr
                                            sy-abcde+18(1). "S - Saída
**<<<------"163355 - NMS - FIM------>>>
        DATA(lt_flote3_aux) = lt_flote2.
        SORT lt_flote3_aux BY docnum_eprod.
        DELETE ADJACENT DUPLICATES FROM lt_flote3_aux COMPARING docnum_eprod.
        SELECT *
          FROM zsdtvinc_p_flote
          INTO TABLE @DATA(lt_flote3)
          FOR ALL ENTRIES IN @lt_flote3_aux
          WHERE docnum_eprod EQ @lt_flote3_aux-docnum_eprod
            AND cancel       EQ @abap_false.
        IF sy-subrc IS INITIAL.
          SORT lt_flote3 BY docnum_flote docnum_eprod ASCENDING.
**<<<------"163355 - NMS - INI------>>>
* Define a Nota Fiscal de Saída conforme RA e o EUDR.
          PERFORM zf_define_nota_saida TABLES lt_flote3
                                        USING i_cod_ra_embarque
                                              i_eudr
                                              i_retornar_eudr
                                              sy-abcde+18(1). "S - Saída
**<<<------"163355 - NMS - FIM------>>>
          lt_flote3_aux = lt_flote3.
          SORT lt_flote3_aux BY docnum_eprod.
          DELETE ADJACENT DUPLICATES FROM lt_flote3_aux COMPARING docnum_eprod.

          SELECT a~id_nomeacao_tran a~id_empresa a~id_filial a~id_material a~id_remetente a~docnum a~itmnum a~grp_retorno a~nr_quantidade
            FROM znom_reme_notas AS a
            INNER JOIN znom_remetente AS b
            ON a~id_nomeacao_tran = b~id_nomeacao_tran
           AND a~id_material = b~id_material
           AND a~id_remetente = b~id_remetente
           AND a~grp_retorno = b~grp_retorno
            INTO TABLE lt_rem_notas
            FOR ALL ENTRIES IN lt_flote3_aux
**<<<------"163355 - NMS - INI------>>>
*            WHERE a~docnum = lt_flote3_aux-docnum_eprod.
            WHERE a~docnum             EQ lt_flote3_aux-docnum_eprod
              AND a~codigo_ra_embarque EQ i_cod_ra_embarque.
**<<<------"163355 - NMS - FIM------>>>
*              AND b~docnum_rt = space.
          IF sy-subrc IS INITIAL.
            SORT lt_rem_notas BY docnum.
          ENDIF.

          SELECT a~docnum a~docdat b~itmnum b~werks b~cfop b~menge
            FROM j_1bnfdoc AS a
            INNER JOIN j_1bnflin AS b
            ON a~docnum = b~docnum
            INTO TABLE lt_j_1bnflin
            FOR ALL ENTRIES IN lt_flote3_aux
            WHERE a~docnum = lt_flote3_aux-docnum_flote.

        ENDIF.

      ENDIF.

*      SELECT *
*        FROM zsdt_retlote
*        INTO TABLE @DATA(lt_retlote)
*        FOR ALL ENTRIES IN @lt_flote_aux
*        WHERE docnum = @lt_flote_aux-docnum_flote.
*      IF sy-subrc IS INITIAL.
*        SORT lt_retlote BY docnum.
*
*        LOOP AT lt_retlote ASSIGNING FIELD-SYMBOL(<fs_retlote>).
*
*          ls_retlote_sum-docnum = <fs_retlote>-docnum.
*          ls_retlote_sum-qtd    = <fs_retlote>-quant_vinc.
*
*          COLLECT ls_retlote_sum INTO lt_retlote_sum.
*          CLEAR ls_retlote_sum.
*
*        ENDLOOP.
*      ENDIF.

      DATA(lt_flote_aux2) = lt_flote.
      SORT lt_flote_aux2 BY docnum_eprod.
      DELETE ADJACENT DUPLICATES FROM lt_flote_aux2 COMPARING docnum_eprod.

      SELECT a~id_nomeacao_tran a~id_empresa a~id_filial a~id_material a~id_remetente a~docnum a~itmnum a~grp_retorno a~nr_quantidade
        FROM znom_reme_notas AS a
        INNER JOIN znom_remetente AS b
        ON a~id_nomeacao_tran = b~id_nomeacao_tran
       AND a~id_material = b~id_material
       AND a~id_remetente = b~id_remetente
       AND a~grp_retorno = b~grp_retorno
        APPENDING TABLE lt_rem_notas
        FOR ALL ENTRIES IN lt_flote_aux2
**<<<------"163355 - NMS - INI------>>>
*        WHERE a~docnum = lt_flote_aux2-docnum_eprod.
        WHERE a~docnum             EQ lt_flote_aux2-docnum_eprod
          AND a~codigo_ra_embarque EQ i_cod_ra_embarque.
**<<<------"163355 - NMS - FIM------>>>
*          AND b~docnum_rt = space.
      IF sy-subrc IS INITIAL.
        SORT lt_rem_notas BY docnum.
      ENDIF.

      SORT lt_rem_notas BY id_nomeacao_tran id_empresa id_filial id_material id_remetente docnum itmnum grp_retorno.
      DELETE ADJACENT DUPLICATES FROM lt_rem_notas COMPARING id_nomeacao_tran id_empresa id_filial id_material id_remetente docnum itmnum grp_retorno.

      SORT lt_rem_notas BY docnum.

      LOOP AT lt_rem_notas ASSIGNING FIELD-SYMBOL(<fs_rem_notas>).

        ls_rem_notas_sum-docnum        = <fs_rem_notas>-docnum.
        ls_rem_notas_sum-nr_quantidade = <fs_rem_notas>-nr_quantidade.

        COLLECT ls_rem_notas_sum INTO lt_rem_notas_sum.
        CLEAR ls_rem_notas_sum.

      ENDLOOP.

    ENDIF.

    IF lt_flote_aux IS NOT INITIAL.

      SELECT a~docnum a~docdat b~itmnum b~werks b~cfop b~menge
        FROM j_1bnfdoc AS a
        INNER JOIN j_1bnflin AS b
        ON a~docnum = b~docnum
        APPENDING TABLE lt_j_1bnflin
        FOR ALL ENTRIES IN lt_flote_aux
        WHERE a~docnum = lt_flote_aux-docnum_flote.
      IF sy-subrc IS INITIAL.
        SORT lt_j_1bnflin BY docnum.

        LOOP AT lt_flote ASSIGNING FIELD-SYMBOL(<fs_flote>).

          READ TABLE lt_j_1bnflin ASSIGNING FIELD-SYMBOL(<fs_j_1bnflin>)
          WITH KEY docnum = <fs_flote>-docnum_flote
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            READ TABLE lt_notas_fiscais_alv ASSIGNING FIELD-SYMBOL(<fs_notas_fiscais_alv>)
            WITH KEY docnum = <fs_flote>-docnum_eprod
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              ls_alv_vinc_lotes-docnum = <fs_notas_fiscais_alv>-docnum.
              ls_alv_vinc_lotes-itmnum = <fs_notas_fiscais_alv>-itmnum.
            ENDIF.

            ls_alv_vinc_lotes-docnum_rfl = <fs_flote>-docnum_flote.
            ls_alv_vinc_lotes-item_rfl = <fs_j_1bnflin>-itmnum.
            ls_alv_vinc_lotes-filial_rfl = <fs_j_1bnflin>-werks.
            ls_alv_vinc_lotes-cfop_rfl = <fs_j_1bnflin>-cfop.
            ls_alv_vinc_lotes-qtd_rfl  = <fs_j_1bnflin>-menge.
            ls_alv_vinc_lotes-data_rfl  = <fs_j_1bnflin>-docdat.

            READ TABLE e_rfl ASSIGNING FIELD-SYMBOL(<fs_alv_vinc_lotes>)
            WITH KEY docnum_rfl = <fs_flote>-docnum_flote.
            IF sy-subrc IS NOT INITIAL.

              READ TABLE lt_retlote_sum ASSIGNING FIELD-SYMBOL(<fs_retlote_sum>)
              WITH KEY docnum = <fs_flote>-docnum_flote
              BINARY SEARCH.
              IF sy-subrc IS INITIAL.
                lv_saldo_fisico = <fs_j_1bnflin>-menge - <fs_retlote_sum>-qtd.
              ELSE.
                lv_saldo_fisico = <fs_j_1bnflin>-menge.
              ENDIF.

              lv_saldo = <fs_j_1bnflin>-menge.

              READ TABLE lt_saldo_eprod TRANSPORTING NO FIELDS
              WITH KEY docnum = <fs_flote>-docnum_eprod.
              IF sy-subrc IS NOT INITIAL.

                IF lv_docnum_eprod_ant IS NOT INITIAL AND
                  lv_docnum_eprod_ant EQ <fs_flote>-docnum_eprod
                  AND lv_qtd_rem <= 0.

                  lv_pular = abap_true.
                  CLEAR lv_qtd_rem.

                ENDIF.
              ENDIF.

              APPEND INITIAL LINE TO lt_rem_saldo ASSIGNING FIELD-SYMBOL(<fs_rem_saldo>).
              <fs_rem_saldo>-docnum = <fs_flote>-docnum_flote.
              <fs_rem_saldo>-saldo  = lv_saldo.

*** Busca de quantidade para outros eprods
              DATA(lt_vinc2) = lt_flote2.
              DELETE lt_vinc2 WHERE docnum_flote <> <fs_flote>-docnum_flote.
**<<<------"165575 - NMS - INI------>>>
* Ordenação por conta da leitura da TI com busca binária.
              SORT lt_flote3 BY docnum_eprod docnum_flote.
**<<<------"165575 - NMS - FIM------>>>
              LOOP AT lt_vinc2 ASSIGNING FIELD-SYMBOL(<fs_vinc2>).

                READ TABLE lt_flote3 TRANSPORTING NO FIELDS
                WITH KEY docnum_eprod = <fs_vinc2>-docnum_eprod
                BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  LOOP AT lt_flote3 ASSIGNING FIELD-SYMBOL(<fs_flote3>) FROM sy-tabix.
                    IF <fs_flote3>-docnum_eprod <> <fs_vinc2>-docnum_eprod OR
                       lv_sair = abap_true.
                      CLEAR lv_sair.
                      EXIT.
                    ENDIF.

                    READ TABLE lt_j_1bnflin ASSIGNING <fs_j_1bnflin>
                    WITH KEY docnum = <fs_flote3>-docnum_flote
                    BINARY SEARCH.
                    IF sy-subrc IS INITIAL.
                      READ TABLE lt_rem_notas_sum ASSIGNING FIELD-SYMBOL(<fs_rem_notas_sum>)
                      WITH KEY docnum = <fs_flote3>-docnum_eprod
                      BINARY SEARCH.
                      IF sy-subrc IS INITIAL.
                        IF lv_qtd_rem2 IS INITIAL.
                          lv_qtd_rem2 = <fs_rem_notas_sum>-nr_quantidade.
                        ENDIF.

                        ls_rem_saldo-docnum = <fs_flote3>-docnum_flote.

                        READ TABLE lt_rem_saldo ASSIGNING <fs_rem_saldo>
                        WITH KEY docnum =  <fs_flote3>-docnum_flote.
                        IF sy-subrc IS INITIAL.

                          IF lv_qtd_rem2 > <fs_flote3>-qtd_vinc.
                            <fs_rem_saldo>-saldo = <fs_rem_saldo>-saldo - <fs_flote3>-qtd_vinc.

                            APPEND INITIAL LINE TO lt_saldo_eprod ASSIGNING FIELD-SYMBOL(<fs_saldo_eprod>).
                            <fs_saldo_eprod>-docnum = <fs_flote3>-docnum_eprod.
                            <fs_saldo_eprod>-saldo  = lv_qtd_rem2 - <fs_flote3>-qtd_vinc.

                            lv_qtd_rem2 = lv_qtd_rem2 - <fs_flote3>-qtd_vinc.

                          ELSEIF lv_qtd_rem2 < <fs_flote3>-qtd_vinc.
                            <fs_rem_saldo>-saldo = <fs_rem_saldo>-saldo - lv_qtd_rem2.
                            lv_qtd_rem2 = lv_qtd_rem2 - <fs_rem_saldo>-saldo.

                          ELSE.

                            <fs_rem_saldo>-saldo = <fs_rem_saldo>-saldo - lv_qtd_rem2.
                            lv_qtd_rem2 = 0.

                          ENDIF.

                        ELSE.

                          IF lv_qtd_rem2 > <fs_flote3>-qtd_vinc.
                            ls_rem_saldo-saldo = <fs_j_1bnflin>-menge - <fs_flote3>-qtd_vinc.

                            APPEND INITIAL LINE TO lt_saldo_eprod ASSIGNING <fs_saldo_eprod>.
                            <fs_saldo_eprod>-docnum = <fs_flote3>-docnum_eprod.
                            <fs_saldo_eprod>-saldo  = lv_qtd_rem2 - <fs_flote3>-qtd_vinc.

                            lv_qtd_rem2 = lv_qtd_rem2 - <fs_flote3>-qtd_vinc.

                          ELSEIF lv_qtd_rem2 < <fs_flote3>-qtd_vinc..
                            ls_rem_saldo-saldo = <fs_j_1bnflin>-menge  - lv_qtd_rem2.
                            lv_qtd_rem2 = lv_qtd_rem2 - <fs_flote3>-qtd_vinc.

                          ELSE.

                            ls_rem_saldo-saldo = <fs_j_1bnflin>-menge  - lv_qtd_rem2.
                            lv_qtd_rem2 = 0.

                          ENDIF.

                          APPEND ls_rem_saldo TO lt_rem_saldo.
                          CLEAR ls_rem_saldo.

                        ENDIF.

                        IF lv_qtd_rem2 <= 0.
                          lv_sair = abap_true.
                        ENDIF.
                      ENDIF.
                      ls_rem_saldo-docnum = <fs_flote3>-docnum_flote.


                    ENDIF.

                  ENDLOOP.

                  CLEAR lv_qtd_rem2.

                ENDIF.

              ENDLOOP.

              READ TABLE lt_rem_saldo ASSIGNING <fs_rem_saldo>
              WITH KEY docnum = <fs_flote>-docnum_flote.
              IF sy-subrc IS INITIAL.
                lv_saldo = <fs_rem_saldo>-saldo.
              ELSE.
                lv_saldo = ls_alv_vinc_lotes-qtd_rfl.
              ENDIF.

            ELSE.

              lv_saldo = <fs_alv_vinc_lotes>-saldo_rfl.
              READ TABLE e_rfl_saldo_fisico ASSIGNING FIELD-SYMBOL(<fs_saldo_fisico>)
              WITH KEY docnum = <fs_flote>-docnum_flote.
              IF sy-subrc IS INITIAL.
                lv_saldo_fisico = <fs_saldo_fisico>-saldo_fisico.
              ENDIF.

            ENDIF.

          ENDIF.

          APPEND INITIAL LINE TO e_rfl_saldo_fisico ASSIGNING <fs_saldo_fisico>.
          MOVE-CORRESPONDING  ls_alv_vinc_lotes TO <fs_saldo_fisico>.
          <fs_saldo_fisico>-saldo_fisico = lv_saldo_fisico.
          <fs_saldo_fisico>-qtd_vinc = <fs_flote>-qtd_vinc.

          IF lv_saldo <= 0.

            CLEAR lv_saldo.
**<<<------"163355 - NMS - INI------>>>
            CLEAR lv_sair.
**<<<------"163355 - NMS - FIM------>>>
            FREE: lt_rem_saldo.
            CONTINUE.

          ENDIF.

          ls_alv_vinc_lotes-saldo_rfl = lv_saldo.

          APPEND ls_alv_vinc_lotes TO e_rfl.
          CLEAR: ls_alv_vinc_lotes,
                 lv_saldo,
                 lv_pular,
                 lv_saldo_fisico.
**<<<------"163355 - NMS - INI------>>>
          CLEAR lv_sair.
**<<<------"163355 - NMS - FIM------>>>
          FREE: lt_rem_saldo.

          lv_docnum_eprod_ant = <fs_flote>-docnum_eprod.

        ENDLOOP.

      ENDIF.

    ENDIF.
**<<<------"165835 - NMS - INI------>>>
    TABLES: zlest0146.
    LOOP AT e_rfl INTO ls_alv_vinc_lotes.
      DATA(vl_tabix) = sy-tabix.
* Busca a data de recepção.
      CALL FUNCTION 'ZCCT_DADOS_RECEPCAO_CARGA'
        EXPORTING
          i_docnum    = ls_alv_vinc_lotes-docnum_rfl
        IMPORTING
          e_zlest0146 = zlest0146.

      ls_alv_vinc_lotes-dt_recepcao = zlest0146-dt_recepcao.
      MODIFY e_rfl FROM ls_alv_vinc_lotes INDEX vl_tabix TRANSPORTING dt_recepcao.

      READ TABLE e_rfl_saldo_fisico ASSIGNING <fs_saldo_fisico> WITH KEY docnum     = ls_alv_vinc_lotes-docnum
                                                                         docnum_rfl = ls_alv_vinc_lotes-docnum_rfl.

      IF sy-subrc IS INITIAL.
        <fs_saldo_fisico>-dt_recepcao = zlest0146-dt_recepcao.

      ENDIF.

    ENDLOOP.
**<<<------"165835 - NMS - FIM------>>>
  ENDFUNCTION.
