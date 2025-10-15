FUNCTION znfw_estorna_seq_lcto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_SEQ_LCTO) TYPE  ZFIWRT0008-SEQ_LCTO OPTIONAL
*"     REFERENCE(I_ESTORNO) TYPE  CHAR1 OPTIONAL
*"  TABLES
*"      T_DOCS STRUCTURE  ZFIWRS0003
*"----------------------------------------------------------------------
  TYPES: BEGIN OF ty_obj_key,
           seq_lcto TYPE zfiwrt0008-seq_lcto,
           obj_key  TYPE zib_contabil_chv-obj_key,
         END OF ty_obj_key.

  DATA: tg_0001        TYPE TABLE OF zfiwrt0001 WITH HEADER LINE,
        tl_0008        TYPE TABLE OF zfiwrt0008 WITH HEADER LINE,
        tl_0008_aux    TYPE TABLE OF zfiwrt0008 WITH HEADER LINE,
        tl_0009        TYPE TABLE OF zfiwrt0009 WITH HEADER LINE,
        tl_0011        TYPE TABLE OF zfiwrt0011 WITH HEADER LINE,
        tl_0012        TYPE TABLE OF zfiwrt0012 WITH HEADER LINE,
        tl_0020        TYPE TABLE OF zfiwrt0020 WITH HEADER LINE,
        tl_zib_cont    TYPE TABLE OF zib_contabil WITH HEADER LINE,
        tl_obj_key     TYPE TABLE OF ty_obj_key WITH HEADER LINE,
        tl_zib_chv     TYPE TABLE OF zib_contabil_chv WITH HEADER LINE,
        tl_1bnfdoc     TYPE TABLE OF j_1bnfdoc WITH HEADER LINE,
        tl_mseg        TYPE TABLE OF mseg WITH HEADER LINE,
        wl_doc_est     TYPE zfiwrt0008-docnum,
        tl_j1baa       TYPE TABLE OF j_1baa WITH HEADER LINE,
        wl_tabix       TYPE sy-tabix,
        wl_gm_head_ret TYPE bapi2017_gm_head_ret,
        tl_return      TYPE TABLE OF bapiret2,
        tl_zib         TYPE TABLE OF zib_contabil WITH HEADER LINE,
        wl_cont        TYPE sy-tabix,
        wl_obj_key     TYPE zib_contabil-obj_key,
        wl_objkey_imp  TYPE zib_contabil-obj_key,
        vl_docnum      TYPE j_1bnfe_active-docnum,
        vl_data_val    TYPE sy-datum,
        vbudat2        TYPE bsis-budat,
        i_doc          TYPE j_1bnfdoc,
        i_acttab       TYPE j_1bnfe_active,
        sano(4),
        vano           TYPE i,
        smes(2),
        vmes           TYPE i.

  DATA: t_hkont TYPE STANDARD TABLE OF  rgsb4 WITH HEADER LINE,
        v_vbund TYPE bseg-vbund.

  DATA: _vbeln TYPE vbeln,
        _cvbrp TYPE vbrpvb.

  DATA: lw_setleaf TYPE setleaf.

  DATA: sl_hdata    TYPE bapiobdlvhdrchg,
        sl_hcont    TYPE bapiobdlvhdrctrlchg,
        vl_delivery TYPE bapiobdlvhdrchg-deliv_numb,
        tl_bapiret2 TYPE bapiret2_t,

        fp_budat    TYPE sy-datlo,
        fp_tcode    TYPE sy-tcode,
        fp_vbtyp    TYPE likp-vbtyp,
        it_mesg     TYPE STANDARD TABLE OF mesg.

  DATA: wl_status        TYPE char0001,
        vl_mensagem(150) TYPE c,
        wl_messa         TYPE char0064.


  DATA: vl_docsta      TYPE j_1bnfe_active-docsta,
        vl_scssta      TYPE j_1bnfe_active-scssta,
        vl_action_requ TYPE j_1bnfe_active-action_requ.

*** INI - STEFANINI - IR173353 - 12.04.2024
  DATA lr_katyp TYPE RANGE OF cskb-katyp.

  lr_katyp = VALUE #( ( sign = 'I'
                      option = 'EQ'
                      low = '11')
                     ( sign = 'I'
                      option = 'EQ'
                      low = '12')  ).
*** END - STEFANINI - IR173353 - 12.04.2024

  CLEAR: wl_doc_est, wl_tabix, wl_gm_head_ret, tl_zib, wl_cont, wl_obj_key.
  REFRESH: tl_0008, tl_return, tl_zib, tl_0009, tl_0011,
           tl_zib_cont, tl_obj_key, tl_zib_chv,
           tl_1bnfdoc, tl_mseg, tl_0008_aux.

  IF i_seq_lcto IS NOT INITIAL.
    MOVE: i_seq_lcto TO t_docs-seq_lcto.

    APPEND t_docs.
    CLEAR: t_docs.
  ENDIF.


  IF t_docs[] IS NOT INITIAL.
    SELECT *
      FROM zfiwrt0008
      INTO TABLE tl_0008
       FOR ALL ENTRIES IN t_docs
       WHERE seq_lcto EQ t_docs-seq_lcto
         AND loekz    EQ space.

    IF sy-subrc IS INITIAL.
      tl_0008_aux[] = tl_0008[].
      DELETE tl_0008_aux WHERE docnum IS INITIAL.
      IF tl_0008_aux[] IS NOT INITIAL.
        SELECT *
          FROM j_1bnfdoc
          INTO TABLE tl_1bnfdoc
           FOR ALL ENTRIES IN tl_0008_aux
           WHERE docref EQ tl_0008_aux-docnum
             AND doctyp EQ '5'.

        SELECT *
          FROM zfiwrt0001
          INTO TABLE tg_0001
          FOR ALL ENTRIES IN tl_0008_aux
          WHERE operacao EQ tl_0008_aux-operacao.
      ENDIF.
      tl_0008_aux[] = tl_0008[].
      DELETE tl_0008_aux WHERE mblnr IS INITIAL.
      IF tl_0008_aux[] IS NOT INITIAL.
        SELECT *
          FROM mseg
          INTO TABLE tl_mseg
           FOR ALL ENTRIES IN tl_0008_aux
           WHERE smbln EQ tl_0008_aux-mblnr
             AND sjahr EQ tl_0008_aux-mjahr.
      ENDIF.
      LOOP AT tl_0008 WHERE obj_key IS NOT INITIAL.
        MOVE: tl_0008-seq_lcto TO tl_obj_key-seq_lcto,
              tl_0008-obj_key  TO tl_obj_key-obj_key.
*      CONCATENATE 'ZGF' TL_0008-seq_lcto TL_0008-budat(4) INTO tg_obj_key-obj_key.
        IF tl_obj_key-obj_key IS NOT INITIAL.
          APPEND tl_obj_key.

        ENDIF.
        CLEAR: tl_obj_key.
      ENDLOOP.

      IF tl_obj_key[] IS NOT INITIAL.
        SELECT *
          FROM zib_contabil_chv
          INTO TABLE tl_zib_chv
           FOR ALL ENTRIES IN tl_obj_key
           WHERE obj_key EQ tl_obj_key-obj_key.

        IF sy-subrc IS INITIAL.
          LOOP AT tl_zib_chv.
            READ TABLE tl_0008
              WITH KEY obj_key = tl_zib_chv-obj_key.
            IF sy-subrc IS INITIAL.
              CONCATENATE 'ZGF' tl_zib_chv-belnr tl_0008-budat(4) INTO tl_obj_key-obj_key.
              MOVE: tl_0008-seq_lcto TO tl_obj_key-seq_lcto.
              APPEND tl_obj_key.
            ENDIF.
            CLEAR: tl_0008, tl_zib_chv.
          ENDLOOP.
        ENDIF.

        SELECT *
            FROM zib_contabil
            INTO TABLE tl_zib_cont
             FOR ALL ENTRIES IN tl_obj_key
             WHERE obj_key EQ tl_obj_key-obj_key.

        SELECT *
          FROM zib_contabil_chv
          APPENDING TABLE tl_zib_chv
           FOR ALL ENTRIES IN tl_obj_key
           WHERE obj_key EQ tl_obj_key-obj_key.

*---> 04/07/2023 - Migração S4 - WS
        SORT tl_zib_chv.
*<--- 04/07/2023 - Migração S4 - WS
        DELETE ADJACENT DUPLICATES FROM tl_zib_chv COMPARING ALL FIELDS.
      ENDIF.

      SELECT *
        FROM zfiwrt0009
        INTO TABLE tl_0009
        FOR ALL ENTRIES IN tl_0008
         WHERE seq_lcto EQ tl_0008-seq_lcto.

      SELECT *
        FROM zfiwrt0011
        INTO TABLE tl_0011
        FOR ALL ENTRIES IN tl_0008
         WHERE seq_lcto EQ tl_0008-seq_lcto
           AND estorno  NE space.

      DELETE tl_0011 WHERE dmbtr IS INITIAL.
    ENDIF.

  ENDIF.

  SELECT *
    FROM zfiwrt0012
    INTO TABLE tl_0012
    FOR ALL ENTRIES IN tl_0008
     WHERE seq_lcto EQ tl_0008-seq_lcto.

  SELECT *
    FROM zfiwrt0020
    INTO TABLE tl_0020
    FOR ALL ENTRIES IN tl_0008
     WHERE seq_lcto EQ tl_0008-seq_lcto.

  SELECT *
    FROM j_1baa
    INTO TABLE tl_j1baa
    FOR ALL ENTRIES IN tl_0008
     WHERE nftype EQ tl_0008-nftype.

  SORT tl_j1baa BY nftype.
  DELETE ADJACENT DUPLICATES FROM tl_j1baa COMPARING nftype.

*** INI - STEFANINI - IR173353 - 12.04.2024
  IF tl_0008[] IS NOT INITIAL AND tl_0011[] IS NOT INITIAL.
    READ TABLE tl_0008 INTO DATA(lw_0008) INDEX 1.
    SELECT SINGLE *
      FROM tka02
      INTO @DATA(ls_tka02)
      WHERE bukrs = @lw_0008-bukrs.
    IF sy-subrc = 0.
      SELECT kokrs, kstar, katyp
        FROM cskb
        INTO TABLE @DATA(lt_cskb)
        FOR ALL ENTRIES IN @tl_0011
        WHERE kokrs =   @ls_tka02-kokrs
          AND kstar =   @tl_0011-hkont
          AND datbi >=  @sy-datum.
    ENDIF.
  ENDIF.
*** END - STEFANINI - IR173353 - 12.04.2024

  SORT: tl_1bnfdoc BY docref,
        tl_mseg BY smbln sjahr,
        tl_zib_chv BY obj_key,
        tl_zib_cont BY obj_key,
        tg_0001     BY operacao,
        tl_0009     BY seq_lcto.

*** Inicio CS2020000902 - 13/09/2020 - Pedro Leite
  IF i_estorno IS NOT INITIAL.
    DATA(lit_0008_aux) = tl_0008[].
    LOOP AT lit_0008_aux INTO DATA(lwa_0008_aux).
      CLEAR: wl_status,
             wl_messa.

      CALL FUNCTION 'Z_CONTROLE_FECHAMES'
        EXPORTING
          i_bukrs  = lwa_0008_aux-bukrs
          i_data   = lwa_0008_aux-budat
*         i_dep_resp =
          i_user   = sy-uname
*         i_monat  =
        IMPORTING
          e_status = wl_status
          e_messa  = wl_messa
        EXCEPTIONS
          error    = 1
          OTHERS   = 2.

      IF wl_status EQ 'E'.
        DELETE tl_0008 WHERE seq_lcto EQ lwa_0008_aux-seq_lcto.
        DELETE tl_0009 WHERE seq_lcto EQ lwa_0008_aux-seq_lcto.

        CONCATENATE 'Erro na seq lcto ' lwa_0008_aux-seq_lcto wl_messa INTO vl_mensagem SEPARATED BY space.

        MESSAGE i836(sd) DISPLAY LIKE 'E' WITH vl_mensagem.

      ENDIF.


***/// [CS2020000805] - Ajustar melhorias da transação ZSDT0163 - Set/2021 - Inicio
      IF lwa_0008_aux-form      IS NOT INITIAL AND
         lwa_0008_aux-docnum    IS NOT INITIAL AND
         lwa_0008_aux-tcode_org NE 'ZNFW0009'. "Não valida lançamentos criados pela ZNFW0009 - Operações Mic

        CLEAR: vl_docsta,vl_scssta.

        SELECT SINGLE nfenum
          INTO @DATA(vl_nfenum)
          FROM j_1bnfdoc
          WHERE docnum = @lwa_0008_aux-docnum.

        IF sy-subrc NE 0.
          CLEAR: vl_nfenum.
        ENDIF.

        SELECT SINGLE docsta scssta action_requ
          INTO (vl_docsta,vl_scssta,vl_action_requ)
          FROM j_1bnfe_active
          WHERE docnum = lwa_0008_aux-docnum.

        IF NOT ( ( vl_nfenum IS INITIAL ) OR
                 ( vl_docsta = '1' AND vl_scssta = '2' AND vl_action_requ IS NOT INITIAL ) OR
                 ( vl_docsta = '2' AND vl_scssta = '4' AND vl_action_requ IS NOT INITIAL ) OR
                 ( vl_docsta = '3' AND vl_scssta = '0' AND vl_action_requ IS NOT INITIAL ) OR
                 ( vl_scssta = 'A' AND vl_action_requ IS NOT INITIAL )
                 ).
          DELETE tl_0008 WHERE seq_lcto EQ lwa_0008_aux-seq_lcto.
          DELETE tl_0009 WHERE seq_lcto EQ lwa_0008_aux-seq_lcto.

          MESSAGE i836(sd) DISPLAY LIKE 'E' WITH 'Não permitido estorno' 'do documento com' 'o status atual seq lcto ' lwa_0008_aux-seq_lcto .
        ENDIF.

      ENDIF.
***/// [CS2020000805] - Ajustar melhorias da transação ZSDT0163 - Set/2021 - Fim
    ENDLOOP.
  ENDIF.
*** Fim CS2020000902 - 13/09/2020 - Pedro Leite

  "REMESSAS
  DATA(_not_mark_loekz) = ''.
  IF i_estorno IS NOT INITIAL.
    LOOP AT tl_0009 WHERE vbeln_r IS NOT INITIAL.

      SELECT SINGLE * INTO @DATA(wa_delivery)
        FROM likp
       WHERE vbeln EQ @tl_0009-vbeln_r.

      CASE wa_delivery-vbtyp.
        WHEN 'J'.

          fp_budat    = sy-datlo.
          fp_tcode    = 'VL09'.
          fp_vbtyp    = 'J'.
          vl_delivery = tl_0009-vbeln_r.

          CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE' "VL09  (Picking)
            EXPORTING
              i_vbeln                   = vl_delivery
              i_budat                   = fp_budat
              i_tcode                   = fp_tcode
              i_vbtyp                   = fp_vbtyp
            TABLES
              t_mesg                    = it_mesg
            EXCEPTIONS
              error_reverse_goods_issue = 1
              OTHERS                    = 2.

          IF sy-subrc NE 0.
            _not_mark_loekz = 'X'.
          ENDIF.
          "
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
          "
          WAIT UP TO 2 SECONDS.
*   Deleta Delivery Criado
          sl_hdata-deliv_numb = tl_0009-vbeln_r.
          sl_hcont-deliv_numb = tl_0009-vbeln_r.
          sl_hcont-dlv_del    = 'X'.

          CALL FUNCTION 'BAPI_OUTB_DELIVERY_CHANGE'
            EXPORTING
              header_data    = sl_hdata
              header_control = sl_hcont
              delivery       = vl_delivery
            TABLES
              return         = tl_bapiret2.
          IF sy-subrc = 0.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.
          ELSE.
            _not_mark_loekz = 'X'.
          ENDIF.

        WHEN '7'.

          DATA: lv_aviso TYPE REF TO zcl_aviso_recebimento.
          CREATE OBJECT lv_aviso.
          lv_aviso->set_nr_remessa( i_remessa = tl_0009-vbeln_r ).
          DATA(r_estornou_aviso) = lv_aviso->eliminar( ).
          DATA(r_retorno_aviso) = lv_aviso->get_retorno_eliminar( ).
          CLEAR: lv_aviso.

          IF r_estornou_aviso EQ abap_false.
            _not_mark_loekz = 'X'.
          ENDIF.

      ENDCASE.

    ENDLOOP.
  ENDIF.

*---> 05/07/2023 - Migração S4 - DL
  SORT tl_0012 BY seq_lcto.
*<--- 05/07/2023 - Migração S4 - DL
  "
  LOOP AT tl_0008.
    wl_tabix = sy-tabix.

    DATA(_docs_gerados)   = ''.
    _not_mark_loekz = ''.

    CLEAR: t_docs, tl_j1baa, wl_obj_key, tl_zib_chv, tl_zib_cont.
    MOVE: tl_0008-seq_lcto TO t_docs-seq_lcto,
          tl_0008-docnum   TO t_docs-docnum,
          tl_0008-mblnr    TO t_docs-mblnr  .

    READ TABLE tl_j1baa WITH KEY nftype = tl_0008-nftype.

    "Verifica Periodo Fechado.
    IF i_estorno IS NOT INITIAL.
      CLEAR: vl_data_val, vbudat2, sano, vano, smes, vmes.

      CALL FUNCTION 'Z_RET_DATA_MES_ABERTO'
        EXPORTING
          p_data_ent  = tl_0008-budat
          p_bukrs     = tl_0008-bukrs
        IMPORTING
          p_data_val  = vl_data_val
        EXCEPTIONS
          sem_periodo = 1
          OTHERS      = 2.

      IF vl_data_val+0(6) NE tl_0008-budat+0(6) AND tl_0008-budat+4(2) LE 12.
        CONTINUE. "Periodo Fechado.
      ENDIF.

      "Check Disponibilizada CCT
      IF ( tl_0008-st_cct EQ '01' ) OR ( tl_0008-st_cct EQ '02' ).
        CONTINUE.
      ENDIF.

      TRY.
          zcl_boletim_producao=>zif_boletim_producao~get_instance(
          )->check_permissao_modificacao( i_seq_lcto_znfw = tl_0008-seq_lcto ).
        CATCH zcx_boletim_producao INTO DATA(zcx_bol_prod).
          zcx_bol_prod->published_erro( EXPORTING i_msgty = 'I' i_msgty_display = 'W' ) .
          RETURN.
      ENDTRY.

*      IF TL_0008-BUDAT+4(2) = '12'.
*        VANO = TL_0008-BUDAT(4).
*        ADD 1 TO VANO.
*        SANO = VANO.
*        CONCATENATE SANO '0101' INTO VBUDAT2.
*      ELSE.
*        VMES = TL_0008-BUDAT+4(2).
*        ADD 1 TO VMES.
*        SMES = VMES.
*        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*          EXPORTING
*            INPUT  = SMES
*          IMPORTING
*            OUTPUT = SMES.
*        CONCATENATE TL_0008-BUDAT(4) SMES '01' INTO VBUDAT2.
*      ENDIF.
*
*      CLEAR: VL_DATA_VAL.
*      CALL FUNCTION 'Z_RET_DATA_MES_ABERTO'
*        EXPORTING
*          P_DATA_ENT  = VBUDAT2
*          P_BUKRS     = TL_0008-BUKRS
*        IMPORTING
*          P_DATA_VAL  = VL_DATA_VAL
*        EXCEPTIONS
*          SEM_PERIODO = 1
*          OTHERS      = 2.
*
*      IF VL_DATA_VAL+0(6) NE VBUDAT2+0(6) AND VBUDAT2+4(2) LE 12.
*        CONTINUE. "Periodo Fechado.
*      ENDIF.

    ENDIF.

    IF tl_0008-docnum IS NOT INITIAL.

      _docs_gerados = 'X'.

      READ TABLE tl_1bnfdoc
        WITH KEY docref = tl_0008-docnum
                  BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.

        IF i_estorno IS NOT INITIAL.

          READ TABLE tl_0012 WITH KEY seq_lcto = tl_0008-seq_lcto BINARY SEARCH.
          SELECT SINGLE * FROM setleaf INTO lw_setleaf WHERE setname = 'ZNFW_CATEGORIA_NOTA'
                                                         AND valfrom = tl_0012-bwart.
          IF ( sy-subrc NE 0 ).

            IF ( tl_0008-tcode_org = 'ZNFW0009' ) AND ( tl_0008-form IS NOT INITIAL ).

              CLEAR: i_doc, i_acttab.

              SELECT SINGLE * INTO i_acttab
                 FROM j_1bnfe_active
                WHERE docnum EQ tl_0008-docnum.

              SELECT SINGLE * INTO i_doc
                FROM j_1bnfdoc
               WHERE docnum EQ tl_0008-docnum.

              i_acttab-scssta = '2'.
              i_acttab-cancel = abap_true.

              CALL FUNCTION 'J_1B_NFE_UPDATE_ACTIVE'
                EXPORTING
                  i_acttab  = i_acttab
                  i_doc     = i_doc
                  i_updmode = 'U'.

            ENDIF.

            CALL FUNCTION 'J_1B_NF_DOCUMENT_CANCEL'
              EXPORTING
                doc_number               = tl_0008-docnum
                ref_type                 = space
                ref_key                  = space
                can_dat                  = sy-datum
              IMPORTING
                doc_number               = t_docs-docnum_est
              EXCEPTIONS
                document_not_found       = 1
                cancel_not_possible      = 2
                nf_cancel_type_not_found = 3
                database_problem         = 4
                docum_lock               = 5
                nfe_cancel_simulation    = 6
                OTHERS                   = 7.
            IF sy-subrc EQ 0.

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              "Verificação de Documento Anulado Referênciado
              "CFOP 1206 - Anulação de valor relativo à prestação de serviço de transporte
              "CFOP 2206 - Anulação de valor relativo à prestação de serviço de transporte
              READ TABLE tl_0009 WITH KEY seq_lcto = tl_0008-seq_lcto.

              IF tl_0009-cfop(4) EQ '1206' OR tl_0009-cfop(4) EQ '2206'.
                LOOP AT tl_0020 WHERE seq_lcto EQ tl_0008-seq_lcto.
                  "Anular Documento.
                  CALL METHOD zcl_cte=>set_anular_cte_saida
                    EXPORTING
                      i_docnum = tl_0020-docnum
                      i_anular = abap_false.

*                 "// Diapara o Hedge para cada Docnum
                  SELECT DISTINCT ( refkey )
                    FROM j_1bnflin
                     INTO TABLE @DATA(it_lin)
                      WHERE docnum EQ @tl_0020-docnum.

                  IF sy-subrc IS INITIAL.

                    LOOP AT it_lin INTO DATA(wa_lin).
                      _vbeln = wa_lin-refkey.

                      SELECT SINGLE *
                        FROM vbrk
                        INTO @DATA(_vbrk)
                        WHERE vbeln EQ @_vbeln.

                      IF sy-subrc IS INITIAL.

                        SELECT SINGLE *
                          FROM vbrp
                          INTO @DATA(_vbrp)
                          WHERE vbeln EQ @_vbeln.

                        _vbrk-sfakn = _vbrk-vbeln.

                        MOVE-CORRESPONDING _vbrp TO _cvbrp.

*                     "// Lança uma perna do Hedge no momento da cancelamento da anulação das Notas
*                     "// Hedge Aquaviario
                        zcl_webservice_tx_curva=>hedge_aquaviario(
                          _code = 'VF01'
                          _vbrk = _vbrk
                          _vbrp = _cvbrp
                        ).

                      ENDIF.

                    ENDLOOP.
                  ENDIF.

                ENDLOOP.
              ENDIF.

            ELSE.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            ENDIF.

          ENDIF.
        ENDIF.
      ELSE.
        MOVE : tl_1bnfdoc-docnum TO t_docs-docnum_est.

      ENDIF.

      "Se documento de estorno não foi gerado, não marcar doc. ZNFW para eliminação.
      IF t_docs-docnum_est IS INITIAL.
        _not_mark_loekz = 'X'.
      ENDIF.

    ENDIF.

*   Caso tenha solicitado estorno. Só prossegue se documento Fiscal esteja estornado.
    IF i_estorno IS NOT INITIAL.
      CLEAR: lw_setleaf.

      READ TABLE tl_0012 WITH KEY seq_lcto = tl_0008-seq_lcto BINARY SEARCH.
      IF sy-subrc = 0.
        SELECT SINGLE * FROM setleaf INTO lw_setleaf WHERE setname = 'ZNFW_CATEGORIA_NOTA'
                                                       AND valfrom = tl_0012-bwart.
      ENDIF.

      IF ( t_docs-docnum_est IS INITIAL ) AND ( lw_setleaf IS INITIAL ).
        CONTINUE.
      ENDIF.
    ENDIF.

    IF tl_0008-mblnr IS NOT INITIAL.

      _docs_gerados = 'X'.

      READ TABLE tl_mseg
        WITH KEY smbln = tl_0008-mblnr
                 sjahr = tl_0008-mjahr
                 BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        IF i_estorno IS NOT INITIAL.
          CALL FUNCTION 'BAPI_GOODSMVT_CANCEL'
            EXPORTING
              materialdocument = tl_0008-mblnr
              matdocumentyear  = tl_0008-mjahr
*             GOODSMVT_PSTNG_DATE       =
*             GOODSMVT_PR_UNAME         =
            IMPORTING
              goodsmvt_headret = wl_gm_head_ret
            TABLES
              return           = tl_return.
          IF wl_gm_head_ret-mat_doc IS NOT INITIAL.
            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = 'X'.

            MOVE: wl_gm_head_ret-mat_doc TO t_docs-mblnr_est.
          ENDIF.
        ENDIF.
      ELSE.
        MOVE: tl_mseg-mblnr TO t_docs-mblnr_est.
      ENDIF.

      "Se documento de estorno não foi gerado, não marcar doc. ZNFW para eliminação.
      IF ( t_docs-mblnr_est IS INITIAL ) .
        _not_mark_loekz = 'X'.
      ENDIF.

    ENDIF.

    READ TABLE tl_zib_chv
      WITH KEY obj_key = tl_0008-obj_key
               BINARY SEARCH.

    IF tl_zib_chv-belnr IS NOT INITIAL.
*    and i_estorno is not initial.

      _docs_gerados = 'X'.

      CONCATENATE 'ZGF' tl_zib_chv-belnr tl_0008-budat(4) INTO wl_obj_key.
      READ TABLE tl_zib_cont
        WITH KEY obj_key = wl_obj_key
                   BINARY SEARCH.

      IF sy-subrc IS INITIAL.
        IF tl_zib_cont-rg_atualizado EQ 'S'.
          READ TABLE tl_zib_chv
            WITH KEY obj_key = wl_obj_key
                      BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            MOVE: tl_zib_chv-belnr TO t_docs-belnr_est.
          ELSE.
            _not_mark_loekz = 'X'.
          ENDIF.

        ELSE.
          _not_mark_loekz = 'X'.
        ENDIF.

      ELSE.

        "Se documento de estorno não foi gerado, não marcar doc. ZNFW para eliminação.
        _not_mark_loekz = 'X'.

        IF i_estorno IS NOT INITIAL.
          CLEAR: tl_zib.
          REFRESH: tl_zib.

          "Estorno Doc. Saída Transferência Imobilizado
          IF ( tl_0008-tp_mv_imob = 'T' ) AND ( tl_j1baa-direct = '2' ).

            READ TABLE tl_0009 WITH KEY seq_lcto = tl_0008-seq_lcto BINARY SEARCH.

            DO 2 TIMES.

              CLEAR: tl_zib.

              CLEAR: tl_zib_cont.
              READ TABLE tl_zib_cont
                WITH KEY obj_key       = tl_0008-obj_key
                         seqitem       = sy-index
                         rg_atualizado = 'S'.

              IF sy-subrc NE 0.
                CONTINUE.
              ENDIF.

              tl_zib-obj_key  = wl_obj_key.
              tl_zib-seqitem  = sy-index.
              CONCATENATE 'ZGF' tl_zib_chv-belnr tl_0008-budat(4) INTO tl_zib-xblnr.

              IF sy-index = 1.
                tl_zib-bschl      = '50'.
                tl_zib-gsber      =  tl_0008-move_plant.
              ELSEIF sy-index = 2.
                tl_zib-gsber      =  tl_0009-bwkey.
                tl_zib-bschl      = '40'.
              ENDIF.

              tl_zib-bukrs      = tl_0008-bukrs.
              tl_zib-interface  = '3'.
              tl_zib-bktxt      = 'Gestão Emissão NF'.

              CONCATENATE tl_0008-bldat+6(2) tl_0008-bldat+4(2) tl_0008-bldat(4) INTO tl_zib-bldat SEPARATED BY '.'.
              CONCATENATE tl_0008-budat+6(2) tl_0008-budat+4(2) tl_0008-budat(4) INTO tl_zib-budat SEPARATED BY '.'.


              tl_zib-gjahr      = tl_0008-budat+0(4).
              tl_zib-monat      = tl_0008-budat+4(2).
              tl_zib-blart      = 'WR'.

              IF sy-index = 1.
                tl_zib-hkont      = '121401'.
              ELSEIF sy-index = 2.
                tl_zib-hkont      = '121401'.
              ENDIF.

              tl_zib-wrbtr      = tl_zib_cont-wrbtr.
              tl_zib-dmbtr      = tl_zib_cont-dmbtr.

              IF tl_zib-wrbtr LE 0.
                CONTINUE.
              ENDIF.

              tl_zib-waers      = 'BRL'.
              IF tl_zib-bschl      = '40'.
                tl_zib-bupla      = tl_0008-move_plant.
              ELSE.
                tl_zib-bupla      = tl_0009-bwkey.
              ENDIF.
              tl_zib-waers_i    = 'BRL'.
              tl_zib-rg_atualizado  = 'N'.
              APPEND tl_zib.
            ENDDO.

          ELSE.

            CALL FUNCTION 'G_SET_GET_ALL_VALUES'
              EXPORTING
                class         = '0000'
                setnr         = 'CONTAS_EC-CS'
              TABLES
                set_values    = t_hkont
              EXCEPTIONS
                set_not_found = 1
                OTHERS        = 2.

*-CS2021000595 - 22.06.2021 - JT - inicio
            READ TABLE tl_0011 WITH KEY seq_lcto = tl_0008-seq_lcto.
            IF sy-subrc <> 0.
              SELECT *
                FROM zfiwrt0011
                INTO TABLE tl_0011
               WHERE seq_lcto EQ tl_0008-seq_lcto
                 AND estorno  EQ abap_false.

              DELETE tl_0011 WHERE dmbtr IS INITIAL.

              DATA(l_lancto_espec) = abap_true.
            ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim

            LOOP AT tl_0011 WHERE seq_lcto EQ tl_0008-seq_lcto.

*            IF sy-subrc IS INITIAL.

              CLEAR v_vbund.
              READ TABLE t_hkont WITH KEY from = tl_0011-hkont.
              IF sy-subrc = 0.
                SELECT SINGLE vbund INTO v_vbund FROM kna1 WHERE lifnr = tl_0008-parid.
              ENDIF.

*          CONCATENATE 'ZG' TL_0008-seq_lcto TL_0008-budat(4) INTO tl_zib-obj_key.
              CONCATENATE 'ZGF' tl_zib_chv-belnr tl_0008-budat(4) INTO tl_zib-xblnr.
              ADD 1 TO wl_cont.
              MOVE: wl_cont              TO tl_zib-seqitem,
                    wl_obj_key           TO tl_zib-obj_key,
                    tl_0011-bschl        TO tl_zib-bschl,
                    tl_0008-branch       TO tl_zib-gsber,
                    tl_0008-bukrs        TO tl_zib-bukrs,
                    v_vbund              TO tl_zib-vbund,
                    '3'                  TO tl_zib-interface,
                    'Gestão Emissão NF'  TO tl_zib-bktxt,
*              TL_0008-bldat        TO tl_zib-bldat,
*              TL_0008-budat        TO tl_zib-budat,
                    tl_0008-budat(4)     TO tl_zib-gjahr,
                    tl_0008-budat+4(2)   TO tl_zib-monat,
                    'WR'                 TO tl_zib-blart,
                    tl_0011-hkont        TO tl_zib-hkont,
                    tl_0011-dmbtr        TO tl_zib-wrbtr,
                    'BRL'                TO tl_zib-waers,
                    tl_0011-curha        TO tl_zib-waers_f,
                    tl_0011-dmbe2        TO tl_zib-dmbe2,

                    tl_0008-branch       TO tl_zib-bupla,
*              tg_0011-dmbtr        TO tl_zib-dmbtr,
                    'N'                  TO tl_zib-rg_atualizado,
                    tl_0011-newbw        TO tl_zib-newbw,
                    tl_0011-kostl        TO tl_zib-kostl.

*-CS2021000595 - 22.06.2021 - JT - inicio
              IF l_lancto_espec = abap_true.
                IF     tl_0011-bschl = '40'.
                  tl_zib-bschl  = '50'.
                ELSEIF tl_0011-bschl = '50'.
                  tl_zib-bschl  = '40'.
                ENDIF.
              ENDIF.
*-CS2021000595 - 22.06.2021 - JT - fim

              " modifica GSBER e BUPLA no caso de complemento de ICMS
              " ler primeiro item apenas
              READ TABLE tl_0009 WITH KEY seq_lcto = tl_0008-seq_lcto BINARY SEARCH.
              " verificar transf_icms
              READ TABLE tg_0001 WITH KEY operacao = tl_0008-operacao BINARY SEARCH.
              IF tl_0008-complemento = 'S'.
                IF tg_0001-transf_icms = 'C'.
                  IF tl_0011-bschl = '50'.
                    tl_zib-gsber = tl_0008-parid+6(4).
                    tl_zib-bupla = tl_0008-parid+6(4).
                  ELSEIF tl_0011-bschl = '40'.
                    tl_zib-gsber = tl_0009-bwkey.
                    tl_zib-bupla = tl_0009-bwkey.
                  ENDIF.
                ELSEIF tg_0001-transf_icms = 'D'.
                  IF tl_0011-bschl = '40'.
                    tl_zib-gsber = tl_0008-parid+6(4).
                    tl_zib-bupla = tl_0008-parid+6(4).
                  ELSEIF tl_0011-bschl = '50'.
                    tl_zib-gsber = tl_0009-bwkey.
                    tl_zib-bupla = tl_0009-bwkey.
                  ENDIF.
                ENDIF.
              ENDIF.

*** INI - STEFANINI - IR173353 - 12.04.2024
              TRY.
                  IF lt_cskb[ kokrs  = ls_tka02-kokrs
                              kstar  = tl_0011-hkont ]-katyp IN lr_katyp.
                    tl_zib-prctr = '9900'.
                    tl_zib-matnr =  tl_0009-matnr.
                  ENDIF.
                CATCH cx_sy_itab_line_not_found.
              ENDTRY.
*** END - STEFANINI - IR173353 - 12.04.2024

              CONCATENATE tl_0008-bldat+6(2) tl_0008-bldat+4(2) tl_0008-bldat(4) INTO tl_zib-bldat SEPARATED BY '.'.
              CONCATENATE tl_0008-budat+6(2) tl_0008-budat+4(2) tl_0008-budat(4) INTO tl_zib-budat SEPARATED BY '.'.

              IF tl_zib-wrbtr LT 0.
                MULTIPLY tl_zib-wrbtr BY -1.
                MULTIPLY tl_zib-dmbe2 BY -1.
              ENDIF.
              IF tl_0011-newbw IS INITIAL.
                APPEND tl_zib.

              ELSE.
                LOOP AT tl_0009
                  WHERE seq_lcto EQ tl_0008-seq_lcto.
                  MOVE: tl_0009-anln1 TO tl_zib-anln1,
                        tl_0009-anln2 TO tl_zib-anln2.

                  APPEND tl_zib.
                ENDLOOP.
              ENDIF.

              CLEAR: tl_zib.
*            ENDIF.
            ENDLOOP.

          ENDIF.

          MODIFY zib_contabil FROM TABLE tl_zib.
          CLEAR: wl_obj_key.
        ENDIF.
      ENDIF.

    ENDIF. "IF TL_ZIB_CHV-BELNR IS NOT INITIAL.

    "Estorno Doc. Imposto - Saída Transferência Imobilizado
    IF ( tl_0008-tp_mv_imob = 'T' ) AND ( tl_j1baa-direct = '2' ).

      CLEAR: wl_objkey_imp, tl_zib_chv.
      CONCATENATE tl_0008-obj_key 'I' INTO wl_objkey_imp.

      SELECT SINGLE *
        INTO tl_zib_chv
        FROM zib_contabil_chv
       WHERE obj_key = wl_objkey_imp.

      IF tl_zib_chv-belnr IS NOT INITIAL.

        _docs_gerados = 'X'.

        CONCATENATE 'ZGF' tl_zib_chv-belnr tl_0008-budat(4) 'I' INTO wl_obj_key.

        CLEAR: tl_zib_cont.
        SELECT SINGLE *
          INTO tl_zib_cont
          FROM zib_contabil
         WHERE obj_key = wl_obj_key.

        IF sy-subrc IS INITIAL.
          IF tl_zib_cont-rg_atualizado EQ 'S'.

            SELECT SINGLE *
              INTO tl_zib_chv
              FROM zib_contabil_chv
             WHERE obj_key = wl_obj_key.

            IF sy-subrc IS INITIAL.
              MOVE: tl_zib_chv-belnr TO t_docs-tblnr_est.
            ENDIF.
          ENDIF.

        ELSE.

          "Se documento de estorno não foi gerado, não marcar doc. ZNFW para eliminação.
          _not_mark_loekz = 'X'.

          IF i_estorno IS NOT INITIAL.
            CLEAR: tl_zib.
            REFRESH: tl_zib.

            READ TABLE tl_0009 WITH KEY seq_lcto = tl_0008-seq_lcto BINARY SEARCH.

            DO 2 TIMES.

              CLEAR: tl_zib.

              CLEAR: tl_zib_cont.
              SELECT SINGLE *
                INTO tl_zib_cont
                FROM zib_contabil
               WHERE obj_key       = wl_objkey_imp
                 AND seqitem       = sy-index
                 AND rg_atualizado = 'S'.

              IF sy-subrc NE 0.
                CONTINUE.
              ENDIF.

              tl_zib-obj_key  = wl_obj_key.
              tl_zib-seqitem  = sy-index.
              CONCATENATE 'ZGF' tl_zib_chv-belnr tl_0008-budat(4) 'I' INTO tl_zib-xblnr.

              IF sy-index = 1.
                tl_zib-bschl      = '50'.
                tl_zib-gsber      =  tl_0009-bwkey.
              ELSEIF sy-index = 2.
                tl_zib-gsber      =  tl_0009-bwkey.
                tl_zib-bschl      = '40'.
              ENDIF.

              tl_zib-bukrs      = tl_0008-bukrs.
              tl_zib-interface  = '3'.
              tl_zib-bktxt      = 'Gestão Emissão NF'.

              CONCATENATE tl_0008-bldat+6(2) tl_0008-bldat+4(2) tl_0008-bldat(4) INTO tl_zib-bldat SEPARATED BY '.'.
              CONCATENATE tl_0008-budat+6(2) tl_0008-budat+4(2) tl_0008-budat(4) INTO tl_zib-budat SEPARATED BY '.'.


              tl_zib-gjahr      = tl_0008-budat+0(4).
              tl_zib-monat      = tl_0008-budat+4(2).
              tl_zib-blart      = 'WR'.

              IF sy-index = 1.
                tl_zib-hkont      = '423007'.
              ELSEIF sy-index = 2.
                tl_zib-hkont      = '213000'.
              ENDIF.

              tl_zib-wrbtr      = tl_zib_cont-wrbtr.
              tl_zib-dmbtr      = tl_zib_cont-dmbtr.

              IF tl_zib-wrbtr LE 0.
                CONTINUE.
              ENDIF.

              tl_zib-waers      = 'BRL'.
              IF tl_zib-bschl   = '40'.
                tl_zib-bupla      = tl_0008-move_plant.
              ELSE.
                tl_zib-bupla      = tl_0009-bwkey.
              ENDIF.
              tl_zib-waers_i    = 'BRL'.
              tl_zib-rg_atualizado  = 'N'.
              APPEND tl_zib.
            ENDDO.

            MODIFY zib_contabil FROM TABLE tl_zib.
            CLEAR: wl_obj_key.

          ENDIF. "IF I_ESTORNO IS NOT INITIAL.

        ENDIF.

      ENDIF. "IF TL_ZIB_CHV-BELNR IS NOT INITIAL.

    ENDIF.  " IF ( TL_0008-TP_MOV_IMOB = 'T' ) AND ( TL_J1BAA-DIRECT = '2' ).

    "Se todos documentos foram estornados, marcar doc. znfw
    IF ( _docs_gerados = 'X' ) AND ( _not_mark_loekz IS INITIAL ) AND ( tl_0008-seq_lcto IS NOT INITIAL ) AND ( tl_0008-docs_estornados IS INITIAL ).
      UPDATE zfiwrt0008 SET docs_estornados = 'X'
                            dt_estorno = sy-datum
                      WHERE seq_lcto = tl_0008-seq_lcto.
    ENDIF.

    MODIFY t_docs INDEX wl_tabix.
    CLEAR: t_docs, wl_obj_key, tl_zib_chv, tl_zib_cont.
  ENDLOOP.
ENDFUNCTION.
