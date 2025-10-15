*&---------------------------------------------------------------------*
*& Include          ZMMR196_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_seleciona_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_seleciona_dados .

  DATA: lv_data      TYPE sy-datum,
        lv_data_mseg TYPE sy-datum,
        lr_mat       TYPE RANGE OF mara-matnr,
        lr_mat2      TYPE RANGE OF mara-matnr.

  IF s_resrv[] IS NOT INITIAL.
    SELECT matnr werks SUM( bdmng )
      FROM mdrs
      INTO TABLE t_mdrs
      WHERE rsnum IN s_resrv
      AND   matnr IN s_matnr
      AND   werks IN s_werks
      GROUP BY matnr werks.
  ENDIF.



  SELECT *
    FROM plaf
    INTO TABLE t_plaf
    WHERE plwrk IN s_werks
      AND dispo IN s_plane
      AND matnr IN s_matnr
      AND plscn EQ '0'
      AND pertr GE sy-datum
      AND paart NE 'VP'.
  IF sy-subrc IS INITIAL.
    DATA(lt_plaf) = t_plaf.
    SORT lt_plaf BY matnr plwrk.
    DELETE ADJACENT DUPLICATES FROM lt_plaf COMPARING matnr plwrk.


    SELECT *
      FROM marc
      INTO TABLE t_marc1
      FOR ALL ENTRIES IN lt_plaf
      WHERE matnr = lt_plaf-matnr
        AND werks = lt_plaf-plwrk
        AND dismm IN s_tp_mrv.
    IF sy-subrc IS INITIAL.
      SORT t_marc1 BY matnr werks.
*      DELETE t_marc1 WHERE mmsta <> space. "DEIXAR BLOQUEADO APARECEREM
    ENDIF.

    lt_plaf = t_plaf.

    SORT lt_plaf BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_plaf COMPARING matnr.

    SELECT *
      FROM mara
      INTO TABLE t_mara1
      FOR ALL ENTRIES IN lt_plaf
      WHERE matnr = lt_plaf-matnr.
    IF sy-subrc IS INITIAL.
      SORT t_mara1 BY matnr.
*      DELETE t_mara1 WHERE mstae <> space. "DEIXAR BLOQUEADO APARECEREM
    ENDIF.

    LOOP AT t_plaf ASSIGNING FIELD-SYMBOL(<fs_plaf>).
      READ TABLE t_marc1 TRANSPORTING NO FIELDS
      WITH KEY matnr = <fs_plaf>-matnr
               werks = <fs_plaf>-plwrk
      BINARY SEARCH.
      IF sy-subrc IS NOT INITIAL.
        DELETE t_plaf WHERE matnr = <fs_plaf>-matnr
                        AND plwrk = <fs_plaf>-plwrk.
      ELSE.
        READ TABLE t_mara1 TRANSPORTING NO FIELDS
        WITH KEY matnr = <fs_plaf>-matnr
        BINARY SEARCH.
        IF sy-subrc IS NOT INITIAL.
          DELETE t_plaf WHERE matnr = <fs_plaf>-matnr.
        ENDIF.
      ENDIF.
    ENDLOOP.

    IF t_plaf IS NOT INITIAL.

      lr_mat = VALUE #( FOR ls_mat IN t_plaf (
                            sign = 'I'
                            option = 'EQ'
                            low    = ls_mat-matnr
                            ) ).
    ENDIF.

  ENDIF.

  DATA(lr_tp_mrv) = s_tp_mrv[].

  IF lr_tp_mrv[] IS  INITIAL.
    APPEND INITIAL LINE TO lr_tp_mrv[] ASSIGNING FIELD-SYMBOL(<fs_tp_mrv>).
    <fs_tp_mrv>-sign   = 'I'.
    <fs_tp_mrv>-option = 'EQ'.
    <fs_tp_mrv>-low    = 'VB'.
  ENDIF.

  IF s_resrv[] IS  INITIAL.
    DELETE lr_tp_mrv WHERE low NE 'VB'.
  ELSEIF t_plaf[] IS INITIAL.
    IF s_matnr[] IS INITIAL.
      MESSAGE 'Não há registros para processar' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      APPEND LINES OF s_matnr TO lr_mat.
    ENDIF.
  ENDIF.

  IF lr_tp_mrv[] IS NOT INITIAL.
    SELECT *
      FROM marc
      INTO TABLE t_marc
      WHERE matnr IN s_matnr
        AND werks IN s_werks
        AND dispo IN s_plane
        AND dismm IN lr_tp_mrv
        AND mabst > 0.
    IF sy-subrc IS INITIAL.
*      DELETE t_marc WHERE mmsta <> space. "DEIXAR BLOQUEADO APARECEREM

      IF t_marc IS NOT INITIAL.

        SORT t_marc BY matnr werks.

        DATA(lt_marc) = t_marc.
        DELETE ADJACENT DUPLICATES FROM lt_marc COMPARING matnr.

        SELECT *
          FROM mara
          APPENDING TABLE t_mara
          FOR ALL ENTRIES IN lt_marc
          WHERE matnr = lt_marc-matnr.
        IF sy-subrc IS INITIAL.
          SORT t_mara BY matnr.

*          DELETE t_mara WHERE mstae <> space. "DEIXAR BLOQUEADO APARECEREM
          IF t_mara IS INITIAL.
            MESSAGE 'Não há registros para processar' TYPE 'S' DISPLAY LIKE 'E'.
          ELSE.

            LOOP AT t_marc ASSIGNING FIELD-SYMBOL(<fs_marc>).
              READ TABLE t_mara TRANSPORTING NO FIELDS
              WITH KEY matnr = <fs_marc>-matnr
              BINARY SEARCH.
              IF sy-subrc IS NOT INITIAL.
                DELETE t_marc WHERE matnr = <fs_marc>-matnr.
              ENDIF.

            ENDLOOP.

          ENDIF.

        ENDIF.

      ELSE.
        MESSAGE 'Não há registros para processar' TYPE 'S' DISPLAY LIKE 'E'.
      ENDIF.

      IF t_marc IS NOT INITIAL.
        lr_mat2 = VALUE #( FOR ls_matnr2 IN t_marc (
                              sign = 'I'
                              option = 'EQ'
                              low    = ls_matnr2-matnr
                              ) ).
      ENDIF.
    ENDIF.
    APPEND LINES OF lr_mat2 TO lr_mat.
  ENDIF.

  IF lr_mat IS NOT INITIAL.

    SELECT *
      FROM mard
      INTO TABLE t_mard
      WHERE matnr IN lr_mat
        AND werks IN s_werks
        AND lgort IN s_lgort.
    IF sy-subrc IS INITIAL.
      SORT t_mard BY matnr werks.
    ENDIF.

    lv_data = sy-datum - 180.
    SELECT *
      FROM eban
      INTO TABLE t_eban
      WHERE matnr IN lr_mat
        AND werks IN s_werks
        AND badat GE lv_data
        AND knttp EQ '' "estoque
        AND loekz EQ ''.
    IF sy-subrc IS INITIAL.
      SORT t_eban BY matnr werks.
      t_eban_konnr[] = t_eban[].
      DELETE t_eban_konnr WHERE konnr_coupa IS INITIAL.

      DATA(lt_eban) = t_eban.
      SORT lt_eban BY banfn bnfpo.
      DELETE ADJACENT DUPLICATES FROM lt_eban COMPARING banfn bnfpo.

      SELECT *
        FROM ekpo
        INTO TABLE @DATA(lt_ekpo)
        FOR ALL ENTRIES IN @lt_eban
        WHERE banfn = @lt_eban-banfn
          AND bnfpo = @lt_eban-bnfpo
          AND knttp = '' "Estoque
*          AND elikz = ''  "Mesmo finalizado o pedido existe para a RC
          AND loekz = ''.
      IF sy-subrc IS INITIAL.
        SORT lt_ekpo BY banfn bnfpo.

        LOOP AT t_eban ASSIGNING FIELD-SYMBOL(<fs_eban>).

          READ TABLE lt_ekpo TRANSPORTING NO FIELDS
          WITH KEY banfn = <fs_eban>-banfn
                   bnfpo = <fs_eban>-bnfpo
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.
            DELETE t_eban WHERE banfn = <fs_eban>-banfn AND
                                bnfpo = <fs_eban>-bnfpo.
          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

    SELECT *
      FROM ekpo
      INTO TABLE t_ekpo
      WHERE matnr IN lr_mat
        AND werks IN s_werks
        AND loekz EQ space
        AND knttp EQ '' "Estoque
        AND elikz EQ ''
        AND aedat GE lv_data.
    IF sy-subrc IS INITIAL.
      SORT t_ekpo BY matnr werks.

      lt_ekpo = t_ekpo.
      SORT lt_ekpo BY ebeln ebelp.
      DELETE ADJACENT DUPLICATES FROM  lt_ekpo COMPARING ebeln ebelp.

      SELECT *
        FROM ekbe
        INTO TABLE t_ekbe
        FOR ALL ENTRIES IN lt_ekpo
        WHERE ebeln = lt_ekpo-ebeln
          AND ebelp = lt_ekpo-ebelp
          AND vgabe = '1'
          AND shkzg = 'S'                                   "BUG163209
          AND NOT EXISTS ( SELECT * FROM mseg AS e WHERE e~smbln EQ ekbe~belnr AND e~sjahr EQ ekbe~gjahr ). "BUG163209
* INICIO - RRIBEIRO - IR243024 - 09.07.2025 - STEFANINI
*      if sy-subrc is initial.
*        sort lt_ekbe by ebeln ebelp.
*        loop at t_ekpo assigning field-symbol(<fs_ekpo>).
*
*          read table lt_ekbe assigning field-symbol(<fs_ekbe>)
*          with key ebeln = <fs_ekpo>-ebeln
*                   ebelp = <fs_ekpo>-ebelp
*          binary search.
*          if sy-subrc is initial.
*            delete t_ekpo where ebeln = <fs_ekpo>-ebeln
*                            and ebelp = <fs_ekpo>-ebelp.
*          endif.
*
*        endloop.
*      endif.
* FIM - RRIBEIRO - IR243024 - 09.07.2025 - STEFANINI


*** Stefanini - IR259669 - 25/09/2025 - LAZAROSR - Início de Alteração
      SORT t_ekbe BY ebeln ebelp.
*** Stefanini - IR259669 - 25/09/2025 - LAZAROSR - Fim de Alteração

    ENDIF.

    lv_data = sy-datum - 180.

    SELECT *
      FROM resb
      INTO TABLE t_resb
      WHERE matnr IN lr_mat
        AND werks IN s_werks
        AND rsnum IN s_resrv
        AND xloek = space
        AND kzear = space.
*        AND enmng = '0'
*        AND bdter > lv_data.
    IF sy-subrc IS INITIAL.
      SORT t_resb BY matnr werks.

      DATA(lt_resb) = t_resb.
      SORT lt_resb BY rsnum.
      DELETE ADJACENT DUPLICATES FROM lt_resb COMPARING rsnum.

      SELECT *
        FROM rkpf
        INTO TABLE @DATA(lt_rkpf)
        FOR ALL ENTRIES IN @lt_resb
        WHERE rsnum = @lt_resb-rsnum.
      IF sy-subrc IS INITIAL.
        SORT lt_rkpf BY rsnum.

        LOOP AT lt_resb ASSIGNING FIELD-SYMBOL(<fs_resb>).

          READ TABLE lt_rkpf TRANSPORTING NO FIELDS
          WITH KEY rsnum = <fs_resb>-rsnum
          BINARY SEARCH.
          IF sy-subrc IS NOT INITIAL.
            DELETE t_resb WHERE rsnum = <fs_resb>-rsnum.
          ENDIF.
        ENDLOOP.

      ELSE.

        FREE t_resb.

      ENDIF.

    ENDIF.

    lv_data_mseg = sy-datum - 90.

    SELECT  * "matnr, werks, menge
      FROM mseg
      INTO CORRESPONDING FIELDS OF TABLE t_mseg
      WHERE matnr IN lr_mat
        AND werks IN s_werks
        AND ( bwart = '201'
         OR   bwart = '261'
         OR   bwart = 'Z91' ) "Ajuste referente o IR190390 / AOENNING.
        AND budat_mkpf >= lv_data_mseg.
    IF sy-subrc IS INITIAL.
      REFRESH t_mseg_aux.
      LOOP AT t_mseg INTO DATA(w_mseg).
        w_mseg-anomes = w_mseg-budat_mkpf+0(6).
        CLEAR w_mseg-budat_mkpf.
        COLLECT w_mseg INTO t_mseg_aux.
      ENDLOOP.
      t_mseg[] = t_mseg_aux[].
      SORT t_mseg BY matnr werks.
    ENDIF.

    SELECT *
      FROM makt
      INTO TABLE t_makt
      WHERE matnr IN lr_mat.
    IF sy-subrc IS INITIAL.
      SORT t_makt BY matnr.
    ENDIF.

  ENDIF.

  APPEND LINES OF t_marc  TO t_marc1.
  APPEND LINES OF t_mara  TO t_mara1.

  IF t_mara1 IS INITIAL.
    MESSAGE s000(z_mm) WITH TEXT-e05 DISPLAY LIKE 'E'.
  ENDIF.

  SORT t_marc1 BY matnr werks.
  SORT t_mara1 BY matnr.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_processa_dados .

  DATA: ls_agrup    TYPE ty_agrup,
        lv_contrato TYPE tkonn,
        v_cont      TYPE i,
        wl_color    TYPE kkblo_specialcol.

  t_mard_aux[] = t_mard[].
  IF s_lgort IS NOT INITIAL.
    DELETE t_mard_aux WHERE lgort NOT IN s_lgort.
  ENDIF.
  SORT t_mard_aux BY matnr werks.

  SORT t_mdrs BY matnr werks.

  LOOP AT t_plaf ASSIGNING FIELD-SYMBOL(<fs_plaf>).

    ls_agrup-matnr       = <fs_plaf>-matnr.
    ls_agrup-werks       = <fs_plaf>-plwrk.
    ls_agrup-total_neces = <fs_plaf>-gsmng.

    IF s_lgort IS NOT INITIAL.
      READ TABLE t_mard_aux TRANSPORTING NO FIELDS
      WITH KEY matnr = <fs_plaf>-matnr
               werks = <fs_plaf>-plwrk
      BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    READ TABLE t_agrup   TRANSPORTING NO FIELDS
                             WITH KEY matnr = <fs_plaf>-matnr
                             werks = <fs_plaf>-plwrk.
    IF sy-subrc NE 0.
      READ TABLE t_mard TRANSPORTING NO FIELDS
      WITH KEY matnr = <fs_plaf>-matnr
               werks = <fs_plaf>-plwrk
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        LOOP AT t_mard ASSIGNING FIELD-SYMBOL(<fs_mard>) FROM sy-tabix.
          IF <fs_plaf>-matnr <> <fs_mard>-matnr OR
             <fs_plaf>-plwrk <> <fs_mard>-werks.
            EXIT.
          ENDIF.

          ls_agrup-saldo_estoq = ls_agrup-saldo_estoq + <fs_mard>-labst.

        ENDLOOP.
      ENDIF.

      READ TABLE t_eban TRANSPORTING NO FIELDS
      WITH KEY matnr = <fs_plaf>-matnr
               werks = <fs_plaf>-plwrk
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        LOOP AT t_eban ASSIGNING FIELD-SYMBOL(<fs_eban>) FROM sy-tabix.
          IF <fs_eban>-matnr <> <fs_plaf>-matnr OR
             <fs_eban>-werks <> <fs_plaf>-plwrk.
            EXIT.
          ENDIF.

          ls_agrup-saldo_requi = ls_agrup-saldo_requi + <fs_eban>-menge.

        ENDLOOP.

      ENDIF.

      READ TABLE t_ekpo TRANSPORTING NO FIELDS
      WITH KEY matnr = <fs_plaf>-matnr
               werks = <fs_plaf>-plwrk
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        LOOP AT t_ekpo ASSIGNING FIELD-SYMBOL(<fs_ekpo>) FROM sy-tabix.
          IF <fs_ekpo>-matnr <> <fs_plaf>-matnr OR
             <fs_ekpo>-werks <> <fs_plaf>-plwrk.
            EXIT.
          ENDIF.

* INICIO - RRIBEIRO - IR243024 - 09.07.2025 - STEFANINI
*        SORT t_ekbe BY ebeln ebelp.
*        READ TABLE t_ekbe INTO DATA(wa_ekbe_aux) WITH KEY ebeln = <fs_ekpo>-ebeln
*                                                       ebelp = <fs_ekpo>-ebelp.
*
*        IF sy-subrc IS INITIAL.
*          ls_agrup-saldo_pedid = wa_ekbe_aux-menge - <fs_ekpo>-menge.
*        ENDIF.
* FIM - RRIBEIRO - IR243024 - 09.07.2025 - STEFANINI

          ls_agrup-saldo_pedid = ls_agrup-saldo_pedid + <fs_ekpo>-menge.

*** Stefanini - IR259669 - 25/09/2025 - LAZAROSR - Início de Alteração
          PERFORM f_subtrair_saldo USING <fs_ekpo>
                                   CHANGING ls_agrup-saldo_pedid.
*** Stefanini - IR259669 - 25/09/2025 - LAZAROSR - Fim de Alteração

        ENDLOOP.

      ENDIF.

      READ TABLE t_resb TRANSPORTING NO FIELDS
      WITH KEY matnr = <fs_plaf>-matnr
               werks = <fs_plaf>-plwrk
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        LOOP AT t_resb ASSIGNING FIELD-SYMBOL(<fs_resb>) FROM sy-tabix.
          IF <fs_resb>-matnr <> <fs_plaf>-matnr OR
             <fs_resb>-werks <> <fs_plaf>-plwrk.
            EXIT.
          ENDIF.

          ls_agrup-total_reser = ls_agrup-total_reser + ( <fs_resb>-bdmng - <fs_resb>-enmng ).

        ENDLOOP.

      ENDIF.

      READ TABLE t_mseg TRANSPORTING NO FIELDS
      WITH KEY matnr = <fs_plaf>-matnr
               werks = <fs_plaf>-plwrk
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        v_cont = 0.
        LOOP AT t_mseg ASSIGNING FIELD-SYMBOL(<fs_mseg>) FROM sy-tabix.
          IF <fs_mseg>-matnr <> <fs_plaf>-matnr OR
             <fs_mseg>-werks <> <fs_plaf>-plwrk.
            EXIT.
          ENDIF.
          ADD 1 TO v_cont.
          ls_agrup-total_consu = ls_agrup-total_consu + <fs_mseg>-menge.
        ENDLOOP.
        IF v_cont GT 0.
          ls_agrup-total_consu = ls_agrup-total_consu / v_cont.
        ENDIF.

      ENDIF.
    ENDIF.

    COLLECT ls_agrup INTO t_agrup.
    CLEAR ls_agrup.

  ENDLOOP.

  LOOP AT t_marc ASSIGNING FIELD-SYMBOL(<fs_marc>).
    CLEAR ls_agrup.
    READ TABLE t_agrup INTO ls_agrup WITH KEY matnr = <fs_marc>-matnr
                                              werks = <fs_marc>-werks.
    IF sy-subrc = 0.
      CONTINUE.
    ENDIF.

    IF s_lgort IS NOT INITIAL.
      READ TABLE t_mard_aux TRANSPORTING NO FIELDS
      WITH KEY matnr = <fs_marc>-matnr
               werks = <fs_marc>-werks
      BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
    ENDIF.

    ls_agrup-matnr       = <fs_marc>-matnr.
    ls_agrup-werks       = <fs_marc>-werks.

    READ TABLE t_mard TRANSPORTING NO FIELDS
    WITH KEY matnr = <fs_marc>-matnr
             werks = <fs_marc>-werks
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      LOOP AT t_mard ASSIGNING <fs_mard> FROM sy-tabix.
        IF <fs_marc>-matnr <> <fs_mard>-matnr OR
           <fs_marc>-werks <> <fs_mard>-werks.
          EXIT.
        ENDIF.
        ls_agrup-saldo_estoq = ls_agrup-saldo_estoq + <fs_mard>-labst.
      ENDLOOP.
    ENDIF.

    READ TABLE t_eban TRANSPORTING NO FIELDS
    WITH KEY matnr = <fs_marc>-matnr
             werks = <fs_marc>-werks
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      LOOP AT t_eban ASSIGNING <fs_eban> FROM sy-tabix.
        IF <fs_eban>-matnr <> <fs_marc>-matnr OR
           <fs_eban>-werks <> <fs_marc>-werks.
          EXIT.
        ENDIF.

        ls_agrup-saldo_requi = ls_agrup-saldo_requi + <fs_eban>-menge.

      ENDLOOP.

    ENDIF.

    READ TABLE t_ekpo TRANSPORTING NO FIELDS
    WITH KEY matnr = <fs_marc>-matnr
             werks = <fs_marc>-werks
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      LOOP AT t_ekpo ASSIGNING <fs_ekpo> FROM sy-tabix.
        IF <fs_ekpo>-matnr <> <fs_marc>-matnr OR
           <fs_ekpo>-werks <> <fs_marc>-werks.
          EXIT.
        ENDIF.

* INICIO - RRIBEIRO - IR243024 - 09.07.2025 - STEFANINI
*        SORT t_ekbe BY ebeln ebelp.
*        READ TABLE t_ekbe INTO DATA(wa_ekbe) WITH KEY ebeln = <fs_ekpo>-ebeln
*                                                       ebelp = <fs_ekpo>-ebelp.
*
*        IF sy-subrc IS INITIAL.
*          ls_agrup-saldo_pedid = wa_ekbe-menge - <fs_ekpo>-menge.
*        ENDIF.
* FIM - RRIBEIRO - IR243024 - 09.07.2025 - STEFANINI

        ls_agrup-saldo_pedid = ls_agrup-saldo_pedid + <fs_ekpo>-menge.

*** Stefanini - IR259669 - 25/09/2025 - LAZAROSR - Início de Alteração
        PERFORM f_subtrair_saldo USING <fs_ekpo>
                                 CHANGING ls_agrup-saldo_pedid.
*** Stefanini - IR259669 - 25/09/2025 - LAZAROSR - Fim de Alteração

      ENDLOOP.

    ENDIF.

    READ TABLE t_resb TRANSPORTING NO FIELDS
    WITH KEY matnr = <fs_marc>-matnr
             werks = <fs_marc>-werks
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      LOOP AT t_resb ASSIGNING <fs_resb> FROM sy-tabix.
        IF <fs_resb>-matnr <> <fs_marc>-matnr OR
           <fs_resb>-werks <> <fs_marc>-werks.
          EXIT.
        ENDIF.

        ls_agrup-total_reser = ls_agrup-total_reser + <fs_resb>-bdmng.

      ENDLOOP.

    ENDIF.

    READ TABLE t_mseg TRANSPORTING NO FIELDS
    WITH KEY matnr = <fs_marc>-matnr
             werks = <fs_marc>-werks
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      v_cont = 0.
      LOOP AT t_mseg ASSIGNING <fs_mseg> FROM sy-tabix.
        IF <fs_mseg>-matnr <> <fs_marc>-matnr OR
           <fs_mseg>-werks <> <fs_marc>-werks.
          EXIT.
        ENDIF.
        ADD 1 TO v_cont.
        ls_agrup-total_consu = ls_agrup-total_consu + <fs_mseg>-menge.

      ENDLOOP.
      IF v_cont GT 0.
        ls_agrup-total_consu = ls_agrup-total_consu / v_cont.
      ENDIF.

    ENDIF.

    COLLECT ls_agrup INTO t_agrup.
    CLEAR ls_agrup.

  ENDLOOP.

  LOOP AT t_agrup ASSIGNING FIELD-SYMBOL(<fs_agrup>).

    CLEAR lv_contrato.

*    READ TABLE t_mdrs INTO DATA(w_mdrs) WITH KEY matnr = <fs_agrup>-matnr
*                                                werks = <fs_agrup>-werks BINARY SEARCH.
*    IF sy-subrc = 0.
*      <fs_agrup>-total_neces = w_mdrs-bdmng. "atende somente a reserva
*    ENDIF.

    IF p_ctr_at IS NOT INITIAL.

      PERFORM f_busca_dados_coupa USING <fs_agrup>
                               CHANGING lv_contrato.

    ENDIF.

    APPEND INITIAL LINE TO t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>).
    <fs_saida>-werks          = <fs_agrup>-werks.
    <fs_saida>-contrato       = lv_contrato.

    READ TABLE t_marc1 ASSIGNING FIELD-SYMBOL(<fs_marc1>)
    WITH KEY matnr = <fs_agrup>-matnr
             werks = <fs_agrup>-werks
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida>-dispo          = <fs_marc1>-dispo.
      <fs_saida>-dismm          = <fs_marc1>-dismm.
      <fs_saida>-minbe          = <fs_marc1>-minbe.
      <fs_saida>-mabst          = <fs_marc1>-mabst.
      <fs_saida>-mabst          = <fs_marc1>-mabst.
      <fs_saida>-bstrf          = <fs_marc1>-bstrf.
    ENDIF.

    <fs_saida>-matnr          = <fs_agrup>-matnr.

    READ TABLE t_mara1 ASSIGNING FIELD-SYMBOL(<fs_mara1>)
    WITH KEY matnr = <fs_agrup>-matnr
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida>-meins          = <fs_mara1>-meins.
      <fs_saida>-mstae          = <fs_mara1>-mstae.
    ENDIF.

    READ TABLE t_makt ASSIGNING FIELD-SYMBOL(<fs_makt>)
    WITH KEY matnr = <fs_agrup>-matnr
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      <fs_saida>-maktx          = <fs_makt>-maktx.
    ENDIF.

    READ TABLE t_eban_konnr ASSIGNING FIELD-SYMBOL(<fs_konnr>)
    WITH KEY matnr = <fs_agrup>-matnr
             werks = <fs_agrup>-werks BINARY SEARCH.
    IF sy-subrc = 0.
      CLEAR: wl_color.
      wl_color-fieldname = 'MATNR'.
      wl_color-color-col = 6.
      wl_color-color-inv = 6.
      APPEND wl_color TO <fs_saida>-color.
    ENDIF.

    <fs_saida>-total_neces    = <fs_agrup>-total_neces.

    <fs_saida>-saldo_estoq    = <fs_agrup>-saldo_estoq.

    <fs_saida>-saldo_requi    = <fs_agrup>-saldo_requi.

    <fs_saida>-saldo_pedid    = <fs_agrup>-saldo_pedid.

    <fs_saida>-total_reser    = <fs_agrup>-total_reser.

    <fs_saida>-disponivel     = <fs_agrup>-saldo_estoq +
                                <fs_agrup>-saldo_requi +
                                <fs_agrup>-saldo_pedid -
                                <fs_agrup>-total_reser.

    <fs_saida>-total_consu    = <fs_agrup>-total_consu.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_exibe_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_exibe_alv .

  IF t_plaf IS NOT INITIAL OR t_marc IS NOT INITIAL.
    CALL SCREEN '0001'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_criar_requisicao
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_criar_requisicao .


  DATA: it_requisition_items TYPE STANDARD TABLE OF bapiebanc,
        wa_requisition_items TYPE bapiebanc,
        it_return            TYPE STANDARD TABLE OF bapireturn,
        wa_return            TYPE bapireturn,
        lt_extensionin       TYPE TABLE OF bapiparex,
        lt_extensionin2      TYPE TABLE OF bapiparex,
        lo_int               TYPE REF TO zcl_integracao_coupa_req_comp,
        lt_eban              TYPE zmmc_dados_int_coupa_eban.

  DATA: wa_makt TYPE makt,
        wa_mara TYPE mara.

  CONSTANTS lc_service TYPE /ui2/service_name VALUE 'COUPA_INT_ENVIA_REQ_COMPRA'.

  DATA:
    it_lines        TYPE STANDARD TABLE OF tline,          "Tabela para gravar texto do cabeçalho
    wa_lines        TYPE tline,
    x_header        TYPE thead,                            "Tabela-parâmetro para gravar texto do cabeçalho
    w_number        LIKE bapiebanc-preq_no,                "Número da Requisição
    c_e             TYPE c VALUE 'I',
    c_x             TYPE c VALUE 'X',
    lv_ebelp        TYPE ebelp,
    l_valid         TYPE c,
    lv_count        TYPE sy-tabix,
    lv_erro         TYPE c,
    lv_valuepart    TYPE valuepart,
    lv_fornec_coupa TYPE lifnr,
    lt_index_rows   TYPE lvc_t_row.

  CLEAR lv_erro.

  CALL METHOD o_alv->check_changed_data IMPORTING e_valid = l_valid.
  IF l_valid IS NOT INITIAL.

    CALL METHOD o_alv->get_selected_rows
      IMPORTING
        et_index_rows = lt_index_rows.

    IF lt_index_rows IS INITIAL.

      MESSAGE s000(z_mm) WITH TEXT-e04 DISPLAY LIKE 'E'.
      EXIT.

    ENDIF.

    IF gv_comprador IS INITIAL OR
       gv_requisitante IS INITIAL OR
       gv_deposito IS INITIAL OR
       gv_dt_remessa IS INITIAL OR
       gv_un IS INITIAL.
      MESSAGE s000(z_mm) WITH TEXT-e01 TEXT-e02 TEXT-e03 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF gv_fornecedor IS NOT INITIAL.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = gv_fornecedor
        IMPORTING
          output = gv_fornecedor.
    ENDIF.

    REFRESH:it_requisition_items, it_return.
    CLEAR: wa_requisition_items, wa_return.

    SORT t_contratos BY contrato.
    LOOP AT lt_index_rows ASSIGNING FIELD-SYMBOL(<fs_index_rows>).
      ADD 1 TO lv_count.
      READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX <fs_index_rows>-index.
      IF sy-subrc IS INITIAL.
        READ TABLE t_marc1 ASSIGNING FIELD-SYMBOL(<fs_marc1>)
            WITH KEY matnr = <fs_saida>-matnr
                     werks = <fs_saida>-werks.
        IF <fs_marc1>-steuc IS INITIAL.
          MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH |Material { <fs_saida>-matnr } sem NCM |.
          lv_erro = abap_true.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.

    CHECK lv_erro IS INITIAL.

    IF lv_count > 99.
      MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'O limite por requisição é 99'
           'linhas'.
      lv_erro = abap_true.
      EXIT.
    ENDIF.

    CLEAR lv_count.
    LOOP AT lt_index_rows ASSIGNING <fs_index_rows>.
      REFRESH lt_extensionin2.
      ADD 1 TO lv_count.

      READ TABLE t_saida ASSIGNING <fs_saida> INDEX <fs_index_rows>-index.
      IF sy-subrc IS INITIAL.
        IF <fs_saida>-fornec_deter IS NOT INITIAL AND gv_fornecedor IS INITIAL.
          MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Flag Fornec. Determinado marcado,'
          'preenchimento obrigatório' 'do campo Fornecedor!'.
          lv_erro = abap_true.
          EXIT.
        ENDIF.

*-US 153816-22-10-2024-#153816-RJF-Inicio
        IF <fs_saida>-urgencia_neces IS NOT INITIAL.

          READ TABLE it_dd07t INTO DATA(wa_dd07t) WITH KEY domvalue_l = <fs_saida>-urgencia_neces.
          IF sy-subrc IS NOT INITIAL.
            MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Urgência Necessidade,'
            'entrar um valor válido!'.
            lv_erro = abap_true.
            EXIT.
          ENDIF.
        ENDIF.
*-US 153816-22-10-2024-#153816-RJF-Fim

        lv_ebelp = lv_count * 10.

        wa_requisition_items-doc_type   = 'RCS'.                   "Tipo de requisição de compra (P/ Agro sempre NB)
        wa_requisition_items-preq_item  = lv_ebelp.       "N Item
        wa_requisition_items-material   = <fs_saida>-matnr.       "N Material
        wa_requisition_items-short_text = <fs_saida>-maktx.          "Texto Breve Material
        wa_requisition_items-store_loc  = gv_deposito.       "Depósito
        wa_requisition_items-quantity   = <fs_saida>-total_neces.       "Quantidade
        wa_requisition_items-pur_group  = gv_comprador.        "Grupo de Comprador
        wa_requisition_items-plant      = <fs_saida>-werks.             "Centro
        wa_requisition_items-trackingno =  gv_acompanha .
        wa_requisition_items-preq_name  = gv_requisitante.
*      wa_requisition_items-prio_urg   = gv_un.
        wa_requisition_items-mrp_contr = gv_planejador.
        wa_requisition_items-c_amt_bapi = <fs_saida>-price.
        wa_requisition_items-price_unit = 1.

        READ TABLE t_mara ASSIGNING FIELD-SYMBOL(<fs_mara>)
        WITH KEY matnr = <fs_saida>-matnr
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_requisition_items-mat_grp    = <fs_mara>-matkl.          "Grupo de Mercadorias
          wa_requisition_items-unit       = <fs_mara>-meins.          "Unidade do Material
        ENDIF.

        wa_requisition_items-deliv_date = gv_dt_remessa.               "Data da remessa
        wa_requisition_items-del_datcat = 1.                      "Tipo de data da remessa
        APPEND wa_requisition_items TO it_requisition_items.

        APPEND INITIAL LINE TO lt_extensionin2 ASSIGNING FIELD-SYMBOL(<fs_extensionin>).

        <fs_extensionin>-structure = 'BAPI_TE_REQUISITION_ITEM'.
        <fs_extensionin>-valuepart1 = lv_ebelp && gv_un.

        IF <fs_saida>-fornec_deter IS NOT INITIAL.
          lv_fornec_coupa = gv_fornecedor.
        ELSEIF <fs_saida>-contrato IS NOT INITIAL.

          READ TABLE t_contratos ASSIGNING FIELD-SYMBOL(<fs_contratos>)
          WITH KEY contrato = <fs_saida>-contrato
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            lv_fornec_coupa = <fs_contratos>-fornecedor.
          ELSE.
            MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH 'Para o contrato informado (matchcode)'
                                                        'não localizado o fornecedor'.
            lv_erro = abap_true.
            EXIT.
          ENDIF.

        ENDIF.

        IF lv_fornec_coupa IS NOT INITIAL.

          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
            EXPORTING
              input  = lv_fornec_coupa
            IMPORTING
              output = lv_fornec_coupa.

          REFRESH lt_extensionin2.
          APPEND INITIAL LINE TO lt_extensionin2 ASSIGNING <fs_extensionin>.
          <fs_extensionin>-structure = 'BAPI_TE_REQUISITION_ITEM'.
          <fs_extensionin>-valuepart1 = lv_ebelp && gv_un && lv_fornec_coupa.

        ENDIF.

        IF <fs_saida>-contrato IS NOT INITIAL AND lv_fornec_coupa IS NOT INITIAL.
          REFRESH lt_extensionin2.
          APPEND INITIAL LINE TO lt_extensionin2 ASSIGNING <fs_extensionin>.
          <fs_extensionin>-structure = 'BAPI_TE_REQUISITION_ITEM'.
          <fs_extensionin>-valuepart1 = lv_ebelp && gv_un && lv_fornec_coupa && <fs_saida>-contrato.

        ENDIF.
        IF lt_extensionin2[] IS NOT INITIAL.
          APPEND LINES OF lt_extensionin2 TO lt_extensionin.
        ENDIF.

        CLEAR lv_fornec_coupa.
      ENDIF.

    ENDLOOP.

    CHECK lv_erro IS INITIAL.

    CALL FUNCTION 'BAPI_REQUISITION_CREATE'
      IMPORTING
        number            = w_number
      TABLES
        requisition_items = it_requisition_items
        return            = it_return
        extensionin       = lt_extensionin.

    READ TABLE it_return INTO wa_return WITH KEY type = c_e.
    IF sy-subrc EQ 0.

      "Commit
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_x.

      IF w_number IS NOT INITIAL.

        "Grava o cabeçalho
        x_header-tdobject = 'EBANH'.
        x_header-tdname   = w_number.
        x_header-tdid     = 'B01'.
        x_header-tdspras  = sy-langu.

        REFRESH: it_lines.

        APPEND INITIAL LINE TO it_lines ASSIGNING FIELD-SYMBOL(<fs_lines>).
        <fs_lines>-tdformat = '*'.
        <fs_lines>-tdline = gv_justificativa.

        CALL FUNCTION 'SAVE_TEXT'
          EXPORTING
            client          = sy-mandt
            header          = x_header
            savemode_direct = 'X'
          TABLES
            lines           = it_lines
          EXCEPTIONS
            id              = 1
            language        = 2
            name            = 3
            object          = 4
            OTHERS          = 5.

        MESSAGE s000(zwrm001) DISPLAY LIKE 'S' WITH 'Requisição de Compra nº' w_number 'criada.'.

        LOOP AT lt_index_rows ASSIGNING <fs_index_rows>.

          READ TABLE t_saida ASSIGNING <fs_saida> INDEX <fs_index_rows>-index.
          IF sy-subrc IS INITIAL.
            <fs_saida>-rc_criada = w_number.
          ENDIF.

        ENDLOOP.

        COMMIT WORK.

        CALL METHOD o_alv->refresh_table_display.

      ENDIF.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      READ TABLE it_return INTO wa_return INDEX 1.
      MESSAGE s000(zwrm001) DISPLAY LIKE 'E' WITH wa_return-message.

    ENDIF.

  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_dispara_md01
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_dispara_md01 .
  DATA: lv_mode     TYPE c VALUE 'N',
        lt_messages TYPE TABLE OF bdcmsgcoll,
        lv_data     TYPE sy-datum,
        it_zmmt0175 TYPE TABLE OF  zmmt0175,
        wa_zmmt0175 TYPE zmmt0175,
        lr_mat      TYPE RANGE OF mara-matnr.

  IF s_resrv[] IS NOT INITIAL.
    SELECT *
        FROM resb
        INTO TABLE t_resb
        WHERE werks IN s_werks
          AND rsnum IN s_resrv
          AND xloek = space
          AND kzear = space.

    lr_mat = VALUE #( FOR ls_res IN t_resb (
                           sign = 'I'
                           option = 'EQ'
                           low    = ls_res-matnr
                           ) ).

    REFRESH  t_resb.

    s_matnr[]  = lr_mat[].
  ENDIF.

  DELETE FROM plaf
      WHERE plwrk IN s_werks
      AND   dispo IN s_plane
      AND   matnr IN s_matnr
      AND   pertr LT sy-datum.
  DELETE FROM zmmt0175
     WHERE berid IN s_werks
     AND   matnr IN s_matnr
     AND   erdat LT sy-datum
     AND   EXISTS ( SELECT * FROM marc WHERE marc~matnr = zmmt0175~matnr AND werks = zmmt0175~berid AND marc~dismm NE 'ND' AND marc~dispo IN s_plane ).
  COMMIT WORK.

  IF s_matnr[] IS NOT INITIAL OR
     s_plane[] IS NOT INITIAL OR
     s_resrv[] IS NOT INITIAL.
    SELECT * "Apenas os que a MD01não vai processar
      FROM dbvm
      INTO TABLE @DATA(lt_dbvm)
      WHERE berid IN @s_werks
      AND   matnr IN @s_matnr
      AND   gsaen = ''
      AND   akken = ''
      AND   NOT EXISTS ( SELECT * FROM zmmt0175 WHERE zmmt0175~berid = dbvm~berid AND zmmt0175~matnr = dbvm~matnr AND erdat >= @sy-datum ) "Ja rodou uma vez zerados
      AND   NOT EXISTS ( SELECT * FROM plaf WHERE plaf~plwrk = dbvm~berid AND plaf~matnr = dbvm~matnr AND pertr >= @sy-datum ) "Ja rodou uma vez MRP
      AND   EXISTS ( SELECT * FROM marc WHERE marc~matnr = dbvm~matnr AND werks = dbvm~berid AND marc~dismm NE 'ND' AND marc~dispo IN @s_plane )
      AND   EXISTS ( SELECT * FROM mara WHERE mara~matnr = dbvm~matnr AND mara~mstae = '' ).
*      AND   EXISTS ( SELECT * FROM mard WHERE mard~matnr = dbvm~matnr AND werks = dbvm~berid AND mard~lgort IN @s_lgort ).
    IF sy-subrc IS INITIAL.
      LOOP AT lt_dbvm ASSIGNING FIELD-SYMBOL(<fs_dbvm>).
        REFRESH lt_messages.
        PERFORM f_bdcdata USING 'SAPMM61R' '0400' 'X' '' ''.
        PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=ENTER'.
        PERFORM f_bdcdata USING '' '' '' 'RM61R-MATNR' <fs_dbvm>-matnr.
        PERFORM f_bdcdata USING '' '' '' 'RM61R-WERKS' <fs_dbvm>-berid.
        PERFORM f_bdcdata USING '' '' '' 'RM61R-GSAEN' 'X'.
        CALL TRANSACTION 'MD20' USING t_bdcdata MESSAGES INTO lt_messages MODE lv_mode.
        IF sy-subrc IS INITIAL.
          COMMIT WORK AND WAIT.
        ENDIF.
        FREE t_bdcdata.
        SELECT SINGLE * FROM plaf
          INTO @DATA(w_plaf)
          WHERE plwrk = @<fs_dbvm>-berid
          AND matnr   = @<fs_dbvm>-matnr.
        IF sy-subrc NE 0.
          wa_zmmt0175-matnr = <fs_dbvm>-matnr.
          wa_zmmt0175-berid = <fs_dbvm>-berid.
          wa_zmmt0175-erdat = sy-datum.
          APPEND wa_zmmt0175 TO it_zmmt0175.
        ENDIF.
      ENDLOOP.
*      MODIFY zmmt0175 FROM TABLE it_zmmt0175.
*      COMMIT WORK.
    ENDIF.
  ENDIF.


  SELECT *
    FROM t001w
    INTO TABLE @DATA(lt_t001w)
    WHERE werks IN @s_werks.
  IF sy-subrc IS INITIAL.

    LOOP AT lt_t001w ASSIGNING FIELD-SYMBOL(<fs_t001w>).

      lv_data = sy-datum+6(2) && sy-datum+4(2) && sy-datum(4).

      PERFORM f_bdcdata USING 'SAPMM61X' '0100' 'X' '' ''.
      PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '/00'.
      PERFORM f_bdcdata USING '' '' '' 'RM61X-WERKS' <fs_t001w>-werks.
      PERFORM f_bdcdata USING '' '' '' 'RM61X-VERSL' 'NETCH'.
      PERFORM f_bdcdata USING '' '' '' 'RM61X-BANER' '3'.
      PERFORM f_bdcdata USING '' '' '' 'RM61X-LIFKZ' '3'.
      PERFORM f_bdcdata USING '' '' '' 'RM61X-DISER' '1'.
      PERFORM f_bdcdata USING '' '' '' 'RM61X-PLMOD' '3'.
      PERFORM f_bdcdata USING '' '' '' 'RM61X-TRMPL' '1'.
      PERFORM f_bdcdata USING '' '' '' 'RM61X-DISPD' lv_data.
      PERFORM f_bdcdata USING 'SAPMSSY0' '120' 'X' '' ''.
      PERFORM f_bdcdata USING '' '' '' 'BDC_OKCODE' '=XBAC'.

      CALL TRANSACTION 'MD01' USING t_bdcdata MESSAGES INTO lt_messages MODE lv_mode.
      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT.
      ENDIF.
      FREE t_bdcdata.

    ENDLOOP.

  ENDIF.
ENDFORM.

FORM f_bdcdata USING p_prog
                     p_tela
                     p_novo
                     p_campo
                     p_valor.

  APPEND INITIAL LINE TO t_bdcdata ASSIGNING FIELD-SYMBOL(<fs_bdcdata>).

  <fs_bdcdata>-program = p_prog.
  <fs_bdcdata>-dynpro  = p_tela.
  <fs_bdcdata>-dynbegin = p_novo.
  <fs_bdcdata>-fnam     = p_campo.
  <fs_bdcdata>-fval     = p_valor.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_fieldcat
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat .

  DATA: lt_fieldcat TYPE slis_t_fieldcat_alv.

  CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
    EXPORTING
      i_structure_name       = 'ZMM_ALV_MRP'
    CHANGING
      ct_fieldcat            = t_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.
  IF sy-subrc = 0.

    LOOP AT t_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).

      CASE <fs_fieldcat>-fieldname.

        WHEN 'WERKS'.

          <fs_fieldcat>-scrtext_l = 'Centro'.
          <fs_fieldcat>-scrtext_l = 'Centro'.
          <fs_fieldcat>-scrtext_l = 'Centro'.
          <fs_fieldcat>-coltext   =  'Centro'.


        WHEN 'DISPO'.

          <fs_fieldcat>-scrtext_l = 'Plan.'.
          <fs_fieldcat>-scrtext_l = 'Plan.'.
          <fs_fieldcat>-scrtext_l = 'Plan.'.
          <fs_fieldcat>-coltext   = 'Plan.'.

        WHEN 'DISMM'.

          <fs_fieldcat>-scrtext_l = 'Tp'.
          <fs_fieldcat>-scrtext_l = 'Tp'.
          <fs_fieldcat>-scrtext_l = 'Tp '.
          <fs_fieldcat>-coltext   = 'Tp '.

        WHEN 'MATNR'.

          <fs_fieldcat>-scrtext_l = 'Material'.
          <fs_fieldcat>-scrtext_l = 'Material'.
          <fs_fieldcat>-scrtext_l = 'Material'.
          <fs_fieldcat>-coltext   = 'Material'.

        WHEN 'MAKTX'.

          <fs_fieldcat>-scrtext_l = 'Descrição'.
          <fs_fieldcat>-scrtext_l = 'Descrição'.
          <fs_fieldcat>-scrtext_l = 'Descrição'.
          <fs_fieldcat>-coltext   = 'Descrição'.

        WHEN 'MEINS'.

          <fs_fieldcat>-scrtext_l = 'UM'.
          <fs_fieldcat>-scrtext_l = 'UM'.
          <fs_fieldcat>-scrtext_l = 'UM'.
          <fs_fieldcat>-coltext   = 'UM'.

        WHEN 'TOTAL_NECES'.

          <fs_fieldcat>-scrtext_l = 'Entrada/Nec.'.
          <fs_fieldcat>-scrtext_m = 'Entrada/Nec.'.
          <fs_fieldcat>-scrtext_s = 'Entrada/Nec.'.
          <fs_fieldcat>-coltext   = 'Entrada/Nec.'.
          <fs_fieldcat>-edit     = abap_true.
*          <fs_fieldcat>-quantity = abap_true.
          <fs_fieldcat>-qfieldname = 'MEINS'.
          <fs_fieldcat>-datatype = 'QUAN'.
          <fs_fieldcat>-inttype = 'p'.
          <fs_fieldcat>-intlen  = '000010'.
          <fs_fieldcat>-decimals = '000003'.

        WHEN 'SALDO_ESTOQ'.

          <fs_fieldcat>-scrtext_l = 'Saldo'.
          <fs_fieldcat>-scrtext_m = 'Saldo'.
          <fs_fieldcat>-scrtext_s = 'Saldo'.
          <fs_fieldcat>-coltext   = 'Saldo'.
          <fs_fieldcat>-quantity = abap_true.

        WHEN 'SALDO_REQUI'.

          <fs_fieldcat>-scrtext_l = 'RC'.
          <fs_fieldcat>-scrtext_m = 'RC'.
          <fs_fieldcat>-scrtext_s = 'RC'.
          <fs_fieldcat>-coltext   = 'RC'.
          <fs_fieldcat>-quantity  = abap_true.
          <fs_fieldcat>-hotspot   = 'X'.

        WHEN 'SALDO_PEDID'.

          <fs_fieldcat>-scrtext_l = 'PC'.
          <fs_fieldcat>-scrtext_m = 'PC'.
          <fs_fieldcat>-scrtext_s = 'PC'.
          <fs_fieldcat>-coltext   = 'PC'.
          <fs_fieldcat>-quantity = abap_true.
          <fs_fieldcat>-hotspot   = 'X'.

        WHEN 'TOTAL_RESER'.

          <fs_fieldcat>-scrtext_l = 'Reserva'.
          <fs_fieldcat>-scrtext_m = 'Reserva'.
          <fs_fieldcat>-scrtext_s = 'Reserva'.
          <fs_fieldcat>-coltext   = 'Reserva'.
          <fs_fieldcat>-quantity = abap_true.
          <fs_fieldcat>-hotspot   = 'X'.

        WHEN 'DISPONIVEL'.

          <fs_fieldcat>-scrtext_l = 'Disponível'.
          <fs_fieldcat>-scrtext_m = 'Disponível'.
          <fs_fieldcat>-scrtext_s = 'Disponível'.
          <fs_fieldcat>-coltext   = 'Disponível'.
          <fs_fieldcat>-quantity = abap_true.

        WHEN 'MINBE'.

          <fs_fieldcat>-scrtext_l = 'Pt Rep'.
          <fs_fieldcat>-scrtext_m = 'Pt Rep'.
          <fs_fieldcat>-scrtext_s = 'Pt Rep'.
          <fs_fieldcat>-coltext   = 'Pt Rep'.
          <fs_fieldcat>-quantity = abap_true.

        WHEN 'MABST'.

          <fs_fieldcat>-scrtext_l = 'Máximo'.
          <fs_fieldcat>-scrtext_m = 'Máximo'.
          <fs_fieldcat>-scrtext_s = 'Máximo'.
          <fs_fieldcat>-coltext   = 'Máximo'.
          <fs_fieldcat>-quantity = abap_true.

        WHEN 'TOTAL_CONSU'.

          <fs_fieldcat>-scrtext_l = 'Mv Mês'.
          <fs_fieldcat>-scrtext_m = 'Mv Mês'.
          <fs_fieldcat>-scrtext_s = 'Mv Mês'.
          <fs_fieldcat>-coltext   = 'Mv Mês'.
          <fs_fieldcat>-quantity = abap_true.

        WHEN 'URGENCIA_NECES'.

          <fs_fieldcat>-scrtext_l = 'UN'.
          <fs_fieldcat>-scrtext_m = 'UN'.
          <fs_fieldcat>-scrtext_s = 'UN'.
          <fs_fieldcat>-coltext   = 'UN'.
          <fs_fieldcat>-edit      = abap_true.

        WHEN 'CONTRATO'.

          <fs_fieldcat>-scrtext_l = 'Contrato'.
          <fs_fieldcat>-scrtext_m = 'Contrato'.
          <fs_fieldcat>-scrtext_s = 'Contrato'.
          <fs_fieldcat>-coltext   = 'Contrato'.
          <fs_fieldcat>-edit     = abap_true.
          <fs_fieldcat>-f4availabl = abap_true.

        WHEN 'FORNEC_DETER'.

          <fs_fieldcat>-scrtext_l = 'Forn Det'.
          <fs_fieldcat>-scrtext_m = 'Forn Det'.
          <fs_fieldcat>-scrtext_s = 'Forn Det'.
          <fs_fieldcat>-coltext   = 'Forn Det'.
          <fs_fieldcat>-checkbox = abap_true.
          <fs_fieldcat>-edit     = abap_true.
          <fs_fieldcat>-outputlen = 5.

        WHEN 'RC_CRIADA'.

          <fs_fieldcat>-scrtext_l = 'RC Gerada'.
          <fs_fieldcat>-scrtext_m = 'RC Gerada'.
          <fs_fieldcat>-scrtext_s = 'RC Gerada'.
          <fs_fieldcat>-coltext   = 'RC Gerada'.

        WHEN 'BSTRF'.

          <fs_fieldcat>-scrtext_l = 'Valor Arredondado'.
          <fs_fieldcat>-scrtext_m = 'Valor Arredondado'.
          <fs_fieldcat>-scrtext_s = 'Valor Arredondado'.
          <fs_fieldcat>-coltext   = 'Valor Arredondado'.
        WHEN OTHERS.

      ENDCASE.

    ENDLOOP.

  ENDIF.

ENDFORM.

FORM f_clear.

  FREE: t_bdcdata,
        t_saida,
        t_fieldcat,
        t_marc,
        t_mara,
        t_plaf,
        t_ekpo,
        t_eban,
        t_mseg,
        t_agrup.

  CLEAR: gv_comprador,
         gv_fornecedor,
         gv_requisitante,
         gv_un,
         gv_dt_remessa,
         gv_justificativa,
         gv_deposito,
         gv_planejador.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_refresh
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_refresh .
  PERFORM f_seleciona_dados.
  PERFORM f_processa_dados.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_f4_contratos
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> ES_ROW_NO_ROW_ID
*&---------------------------------------------------------------------*
FORM f_monta_f4_contratos  USING  p_row_id _mostra_itens.

  TYPES:
    BEGIN OF ty_contratos,
      contrato       TYPE zcontrato,
      fornecedor     TYPE lifnr,
      nome           TYPE name1,
      cidade         TYPE ort01,
      associar_todos TYPE zmme_associa_todos,
    END OF ty_contratos.

  DATA: lt_retorno      TYPE TABLE OF zmme_retorno_supplier_item,
        lv_string       TYPE string,
        lt_contratos    TYPE TABLE OF zmme_alv_f4_contrato,
        lv_tabix        TYPE sy-tabix,
        lt_return       TYPE TABLE OF ddshretval,
        lv_datum        TYPE timestamp,
        lv_data_conv    TYPE string,
        lt_fieldcat     TYPE slis_t_fieldcat_alv,
        ls_layout       TYPE slis_layout_alv,
        lt_retorno_ctr  TYPE TABLE OF zmme_supp_itens,
        lv_matnr        TYPE matnr,
        lv_matnr18      TYPE matnr18,
        lv_info_request TYPE string,
        lv_offset       TYPE string,
        vg_valido(1).

  SORT t_contratos BY matnr.
  READ TABLE t_saida ASSIGNING FIELD-SYMBOL(<fs_saida>) INDEX p_row_id.
  IF sy-subrc IS INITIAL.
    IF p_ctr_at IS INITIAL.
      DO.
        lv_info_request = <fs_saida>-matnr && ';' && lv_offset.
        lv_offset = lv_offset + 50.
        zcl_int_ob_supplier_item_coupa=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lv_info_request IMPORTING e_integracao = DATA(r_response) ).
        IF r_response IS NOT INITIAL.
          lv_string = r_response-ds_data_retorno.
          REPLACE ALL OCCURRENCES OF '-' IN lv_string WITH '_'.
          /ui2/cl_json=>deserialize( EXPORTING json = lv_string CHANGING data = lt_retorno ).

          IF lt_retorno IS INITIAL.

            CLEAR lv_offset.

            EXIT.

          ENDIF.


          CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP lv_datum TIME ZONE sy-zonlo.

          CALL METHOD cl_xlf_date_time=>create
            EXPORTING
              timestamp = lv_datum
            RECEIVING
              iso8601   = lv_data_conv.

          REPLACE ALL OCCURRENCES OF '-' IN lv_data_conv WITH '_'.

          DELETE lt_retorno WHERE supplier-number = 'none' OR
                                  supplier-number = ' ' OR
                                  contract-status <> 'published' OR
                                  item-active = '' OR
                                  contract-start_date > lv_data_conv.

*                                  OR    contract-end_date < lv_data_conv.

          LOOP AT lt_retorno ASSIGNING FIELD-SYMBOL(<fs_retorno>).
            lv_tabix = sy-tabix.

            DATA(lt_content_group) = <fs_retorno>-contract-content_groups.

            DELETE lt_content_group WHERE name <> <fs_saida>-werks AND name <> 'Everyone'.
            IF lt_content_group IS INITIAL OR ( <fs_retorno>-contract-end_date NE ' ' AND <fs_retorno>-contract-end_date LT lv_data_conv ).
              DELETE lt_retorno INDEX lv_tabix.
            ENDIF.

          ENDLOOP.

          LOOP AT lt_retorno ASSIGNING <fs_retorno>.

            APPEND INITIAL LINE TO lt_contratos ASSIGNING FIELD-SYMBOL(<fs_contratos1>).

            <fs_contratos1>-contrato   = <fs_retorno>-contract-id.
            <fs_contratos1>-fornecedor = <fs_retorno>-supplier-number.
            <fs_contratos1>-price      = <fs_retorno>-price.

            <fs_contratos1>-fornecedor = |{ <fs_contratos1>-fornecedor ALPHA = IN }|.

            SELECT SINGLE name1, ort01
              FROM lfa1
              INTO @DATA(ls_lfa1)
              WHERE lifnr = @<fs_contratos1>-fornecedor.
            IF sy-subrc IS INITIAL.
              <fs_contratos1>-nome       = ls_lfa1-name1.
              <fs_contratos1>-cidade     = ls_lfa1-ort01.
            ENDIF.

            APPEND INITIAL LINE TO t_contratos ASSIGNING FIELD-SYMBOL(<fs_contratos2>).

            <fs_contratos2>-contrato   =  <fs_retorno>-contract-id.
            <fs_contratos2>-fornecedor =  <fs_retorno>-supplier-number.

          ENDLOOP.

        ELSE.

          READ TABLE t_contratos TRANSPORTING NO FIELDS
          WITH KEY matnr = <fs_saida>-matnr
          BINARY SEARCH.
          IF sy-subrc IS INITIAL.

            LOOP AT t_contratos ASSIGNING FIELD-SYMBOL(<fs_contratos>) FROM sy-tabix.
              IF <fs_saida>-matnr <> <fs_contratos>-matnr.
                EXIT.
              ENDIF.

              APPEND INITIAL LINE TO lt_contratos ASSIGNING <fs_contratos1>.

              <fs_contratos1>-contrato   = <fs_contratos>-contrato.
              <fs_contratos1>-fornecedor = <fs_contratos>-fornecedor.
              <fs_contratos1>-nome       = <fs_contratos>-nome.
              <fs_contratos1>-cidade     = <fs_contratos>-cidade.

            ENDLOOP.

          ENDIF.

        ENDIF.
      ENDDO.

      IF lt_contratos IS NOT INITIAL.
********************************************************************** 152612 CS2024000814 - Regra contratos A definir ZMM0223 PSA
        IF _mostra_itens = abap_false.
          DELETE lt_contratos WHERE fornecedor = 'A definir'.
        ENDIF.
**********************************************************************
        CLEAR lv_offset.
        CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
          EXPORTING
            i_structure_name       = 'ZMME_ALV_F4_CONTRATO'
          CHANGING
            ct_fieldcat            = lt_fieldcat
          EXCEPTIONS
            inconsistent_interface = 1
            program_error          = 2
            OTHERS                 = 3.
        IF sy-subrc = 0.

          LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fieldcat>).
            IF <fs_fieldcat>-fieldname EQ 'ASSOCIAR_TODOS'.
              <fs_fieldcat>-edit = abap_true.
              <fs_fieldcat>-checkbox = abap_true.
            ELSEIF <fs_fieldcat>-fieldname EQ 'SEL'.
              <fs_fieldcat>-no_out = abap_true.
            ENDIF.
          ENDLOOP.

          ls_layout-zebra = abap_true.
          ls_layout-colwidth_optimize = abap_true.
          ls_layout-box_fieldname = 'SEL'.

          CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
            EXPORTING
              i_callback_program      = sy-repid
              i_callback_user_command = 'F_USER_COMMAND'
              is_layout               = ls_layout
              it_fieldcat             = lt_fieldcat
              i_screen_start_column   = 10
              i_screen_start_line     = 30
              i_screen_end_column     = 100
              i_screen_end_line       = 50
            TABLES
              t_outtab                = lt_contratos
            EXCEPTIONS
              program_error           = 1
              OTHERS                  = 2.

        ENDIF.

        DATA(lt_contratos_aux) = lt_contratos.
        DELETE lt_contratos_aux WHERE sel IS INITIAL.
        READ TABLE lt_contratos_aux ASSIGNING FIELD-SYMBOL(<fs_contratos_aux>) INDEX 1.
        IF sy-subrc IS INITIAL.
          <fs_saida>-contrato = <fs_contratos_aux>-contrato.
          <fs_saida>-price    = <fs_contratos_aux>-price.

          IF <fs_contratos_aux>-associar_todos IS NOT INITIAL.
            SORT t_saida BY matnr.
            SELECT *
              FROM zmmt0177
              INTO TABLE @DATA(it_zmmt0177)
              WHERE contract_id = @<fs_contratos_aux>-contrato
              AND   ( purchasable = 'true' OR purchasable = 'X' ).
            IF it_zmmt0177[] IS NOT INITIAL.
              LOOP AT it_zmmt0177 INTO DATA(wa_zmmt0177).
                READ TABLE t_saida ASSIGNING <fs_saida>
                                            WITH KEY matnr = wa_zmmt0177-item_number
                                            BINARY SEARCH.
                IF sy-subrc IS INITIAL.
                  IF <fs_saida>-contrato IS INITIAL.
                    CLEAR vg_valido.
                    PERFORM f_check_coupa USING <fs_contratos_aux>-contrato <fs_saida>-matnr CHANGING vg_valido.
                    IF vg_valido = 'X'.
                      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
                        EXPORTING
                          input  = wa_zmmt0177-contract_id
                        IMPORTING
                          output = <fs_saida>-contrato.
                      <fs_saida>-price    = wa_zmmt0177-price.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDLOOP.
            ELSE.
*              do.
*
*                lv_info_request = <fs_contratos_aux>-contrato && ';' && lv_offset.
*
*                lv_offset = lv_offset + 50.
*
*                zcl_int_ob_supplier_item_contr=>zif_integracao_outbound~get_instance( )->execute_request( exporting i_info_request = lv_info_request importing e_integracao = r_response ).
*                if r_response is not initial.
*                  lv_string = r_response-ds_data_retorno.
*                  replace all occurrences of '-' in lv_string with '_'.
*                  /ui2/cl_json=>deserialize( exporting json = lv_string changing data = lt_retorno_ctr ).
*
*                  if lt_retorno_ctr is initial.
*
*                    clear lv_offset.
*
*                    exit.
*
*                  endif.
*
*                  sort t_saida by matnr.
*
*                  loop at lt_retorno_ctr assigning field-symbol(<fs_retorno_ctr>).
*
*                    lv_matnr = |{ <fs_retorno_ctr>-item-item_number alpha = in }|.
*                    lv_matnr18 = |{ <fs_retorno_ctr>-item-item_number alpha = in }|.
*
*                    read table t_saida assigning <fs_saida>
*                    with key matnr = lv_matnr18
*                    binary search.
*                    if sy-subrc is initial.
*
*                      if <fs_saida>-contrato is initial.
*                        <fs_saida>-contrato = <fs_contratos_aux>-contrato.
*                        <fs_saida>-price    = <fs_retorno_ctr>-price.
*                      endif.
*
*                    else.
*
*                      read table t_saida assigning <fs_saida>
*                      with key matnr = lv_matnr
*                      binary search.
*                      if sy-subrc is initial.
*                        if <fs_saida>-contrato is initial.
*                          <fs_saida>-contrato = <fs_contratos_aux>-contrato.
*                          <fs_saida>-price    = <fs_retorno_ctr>-price.
*                        endif.
*
*                      endif.
*
*                    endif.
*
*                  endloop.
*
*                endif.
*
*              enddo.
            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

  CALL METHOD o_alv->refresh_table_display.

ENDFORM.

FORM f_monta_docs  USING  p_row_id p_column_id.
  TYPES: BEGIN OF ty_itab ,
           value     TYPE ebeln,
           name(100) TYPE c,
         END OF ty_itab.

  DATA: msg_alv    TYPE char80,
        itab_msg   TYPE TABLE OF ty_itab,
        wtab_msg   TYPE  ty_itab,
        gw_choice  TYPE sy-tabix,
        w_qtde(16),
        w_qtden    TYPE resb-bdmng,
        vini       TYPE i,
        vfim       TYPE i,
*** Stefanini - IR259669 - 25/09/2025 - LAZAROSR - Início de Alteração
        lv_saldo   TYPE ty_agrup-saldo_pedid.
*** Stefanini - IR259669 - 25/09/2025 - LAZAROSR - Fim de Alteração

  REFRESH itab_msg.
  msg_alv = 'Lista documentos'(113).
  "
  READ TABLE t_saida INTO w_saida INDEX  p_row_id.
  IF sy-subrc = 0.
    IF p_column_id =  'SALDO_REQUI'.
      wtab_msg-value   = 'Requisicao'.
      wtab_msg-name    = '|DATA       | QUANTIDADE'.
      APPEND wtab_msg TO itab_msg .
      CLEAR wtab_msg.
      wtab_msg-value   = '----------'.
      wtab_msg-name    = '---------------------------------------'.
      APPEND wtab_msg TO itab_msg.
      CLEAR wtab_msg.
      LOOP AT t_eban ASSIGNING FIELD-SYMBOL(<fs_eban>) WHERE matnr =  w_saida-matnr
                                                       AND   werks =  w_saida-werks.

        CLEAR wtab_msg.
        WRITE <fs_eban>-menge TO w_qtde.
*        wtab_msg-name+0   = <fs_eban>-banfn.
        wtab_msg-name+0  = '|'.
        wtab_msg-name+1  = <fs_eban>-badat.
        wtab_msg-name+12  = '|'.
        wtab_msg-name+13  = w_qtde.
        wtab_msg-value    = <fs_eban>-banfn..

        APPEND wtab_msg TO itab_msg .
        CLEAR wtab_msg.

      ENDLOOP.
    ELSEIF p_column_id =  'SALDO_PEDID'.
      wtab_msg-value   = 'Pedido'.
      wtab_msg-name    = '|DATA       | QUANTIDADE'.
      APPEND wtab_msg TO itab_msg .
      CLEAR wtab_msg.
      wtab_msg-value   = '----------'.
      wtab_msg-name    = '---------------------------------------'.
      APPEND wtab_msg TO itab_msg.
      CLEAR wtab_msg.
      LOOP AT  t_ekpo  ASSIGNING FIELD-SYMBOL(<fs_ekpo>) WHERE matnr =  w_saida-matnr
                                                         AND   werks =  w_saida-werks.

        CLEAR wtab_msg.

*** Stefanini - IR259669 - 25/09/2025 - LAZAROSR - Início de Alteração
*        WRITE <fs_ekpo>-menge TO w_qtde.

        lv_saldo = <fs_ekpo>-menge.

        PERFORM f_subtrair_saldo USING <fs_ekpo>
                                 CHANGING lv_saldo.

        IF lv_saldo IS INITIAL.
          CONTINUE.
        ENDIF.

        WRITE lv_saldo TO w_qtde.
*** Stefanini - IR259669 - 25/09/2025 - LAZAROSR - Fim de Alteração

*        wtab_msg-name+0   = <fs_ekpo>-ebeln.
        wtab_msg-name+0  = '|'.
        wtab_msg-name+1  = <fs_ekpo>-aedat.
        wtab_msg-name+12  = '|'.
        wtab_msg-name+13  = w_qtde.
        wtab_msg-value    = <fs_ekpo>-ebeln.
        APPEND wtab_msg TO itab_msg .
        CLEAR wtab_msg.

      ENDLOOP.
    ELSEIF p_column_id =  'TOTAL_RESER'.
      wtab_msg-value   = 'Reserva'.
      wtab_msg-name    = '|DATA       | QUANTIDADE'.
      APPEND wtab_msg TO itab_msg .
      CLEAR wtab_msg.
      wtab_msg-value   = '----------'.
      wtab_msg-name    = '---------------------------------------'.
      APPEND wtab_msg TO itab_msg.
      CLEAR wtab_msg.
      LOOP AT  t_resb   ASSIGNING FIELD-SYMBOL(<fs_resb>) WHERE matnr =  w_saida-matnr
                                                          AND   werks =  w_saida-werks.

        CLEAR wtab_msg.
        w_qtden = ( <fs_resb>-bdmng - <fs_resb>-enmng ).
        WRITE w_qtden TO w_qtde.
*        wtab_msg-name+0   = <fs_resb>-rsnum.
        wtab_msg-name+0  = '|'.
        wtab_msg-name+1  = <fs_resb>-bdter.
        wtab_msg-name+12  = '|'.
        wtab_msg-name+13  = w_qtde.
        wtab_msg-value    = <fs_resb>-rsnum.
        APPEND wtab_msg TO itab_msg .
        CLEAR wtab_msg.

      ENDLOOP.
    ENDIF.
  ENDIF.
  vini = 80.
  vfim = 150.
  IF itab_msg[] IS NOT INITIAL.
    CALL FUNCTION 'POPUP_WITH_TABLE_DISPLAY'
      EXPORTING
        endpos_col   = vfim
        endpos_row   = 16
        startpos_col = vini
        startpos_row = 12
        titletext    = msg_alv
      IMPORTING
        choise       = gw_choice
      TABLES
        valuetab     = itab_msg
      EXCEPTIONS
        break_off    = 1
        OTHERS       = 2.
    CHECK gw_choice > 2.
    READ TABLE itab_msg INTO wtab_msg INDEX gw_choice.
    IF wtab_msg-value IS NOT INITIAL.
      IF p_column_id =  'TOTAL_RESER'.
        SET PARAMETER ID 'RES' FIELD wtab_msg-value.
        CALL TRANSACTION 'MB23' AND SKIP FIRST SCREEN.
      ELSEIF p_column_id =  'SALDO_PEDID'.
        SET PARAMETER ID 'BES' FIELD wtab_msg-value..
        CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.
      ELSEIF  p_column_id =  'SALDO_REQUI'.
        SET PARAMETER ID 'BAN' FIELD wtab_msg-value.
        CALL TRANSACTION 'ME53N' AND SKIP FIRST SCREEN.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_busca_dados_coupa
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <FS_AGRUP>
*&---------------------------------------------------------------------*
FORM f_busca_dados_coupa  USING p_fs_agrup TYPE ty_agrup
                       CHANGING p_contrato TYPE tkonn.

  DATA: lt_retorno   TYPE TABLE OF zmme_retorno_supplier_item,
        lv_string    TYPE string,
*        lt_contratos TYPE TABLE OF ty_contratos,
        lv_tabix     TYPE sy-tabix,
        lt_return    TYPE TABLE OF ddshretval,
        lv_datum     TYPE timestamp,
        lv_data_conv TYPE string,
        lv_lifnr     TYPE lifnr,
        lv_lines     TYPE sy-tabix.

  zcl_int_ob_supplier_item_coupa=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = p_fs_agrup-matnr IMPORTING e_integracao = DATA(r_response) ).
  IF r_response IS NOT INITIAL.
    lv_string = r_response-ds_data_retorno.
    REPLACE ALL OCCURRENCES OF '-' IN lv_string WITH '_'.
    /ui2/cl_json=>deserialize( EXPORTING json = lv_string CHANGING data = lt_retorno ).

    CONVERT DATE sy-datum TIME sy-uzeit INTO TIME STAMP lv_datum TIME ZONE sy-zonlo.

    CALL METHOD cl_xlf_date_time=>create
      EXPORTING
        timestamp = lv_datum
      RECEIVING
        iso8601   = lv_data_conv.

    REPLACE ALL OCCURRENCES OF '-' IN lv_data_conv WITH '_'.

    DELETE lt_retorno WHERE supplier-number = 'none' OR
                            contract-status <> 'published' OR
                            item-active = '' OR
                            contract-start_date > lv_data_conv OR
                            contract-end_date < lv_data_conv.

    LOOP AT lt_retorno ASSIGNING FIELD-SYMBOL(<fs_retorno>).
      lv_tabix = sy-tabix.

      DATA(lt_content_group) = <fs_retorno>-contract-content_groups.

      DELETE lt_content_group WHERE name <> p_fs_agrup-werks AND name <> 'Everyone'.
      IF lt_content_group IS INITIAL.
        DELETE lt_retorno INDEX lv_tabix.
      ENDIF.

    ENDLOOP.

    LOOP AT lt_retorno ASSIGNING <fs_retorno>.
*      APPEND INITIAL LINE TO lt_contratos ASSIGNING FIELD-SYMBOL(<fs_contratos>).
*
*      <fs_contratos>-contrato   = <fs_retorno>-contract-id.
*      <fs_contratos>-fornecedor = <fs_retorno>-supplier-number.

      APPEND INITIAL LINE TO t_contratos ASSIGNING FIELD-SYMBOL(<fs_contratos2>).

      <fs_contratos2>-contrato   =  <fs_retorno>-contract-id.
      <fs_contratos2>-fornecedor =  |{ <fs_retorno>-supplier-number ALPHA = IN }|.
      <fs_contratos2>-matnr      =  p_fs_agrup-matnr.

      DESCRIBE TABLE lt_retorno LINES lv_lines.
      IF lv_lines = 1.
        p_contrato = <fs_contratos2>-contrato.
      ENDIF.

    ENDLOOP.

    IF t_contratos IS NOT INITIAL.

      DATA(lt_retorno_aux) = t_contratos.
      SORT lt_retorno_aux BY fornecedor.
      DELETE ADJACENT DUPLICATES FROM lt_retorno_aux COMPARING fornecedor.

      IF lt_retorno_aux IS NOT INITIAL.

        SELECT lifnr, name1, ort01
          FROM lfa1
          INTO TABLE @DATA(lt_lfa1)
          FOR ALL ENTRIES IN @lt_retorno_aux
          WHERE lifnr = @lt_retorno_aux-fornecedor.
        IF sy-subrc IS INITIAL.
          SORT lt_lfa1 BY lifnr.

          LOOP AT t_contratos ASSIGNING <fs_contratos2> .

            READ TABLE lt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>)
            WITH KEY lifnr = <fs_contratos2>-fornecedor
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.

              <fs_contratos2>-nome   = <fs_lfa1>-name1.
              <fs_contratos2>-cidade = <fs_lfa1>-ort01.

            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.

FORM f_user_command USING r_ucomm     LIKE sy-ucomm
                          rs_selfield TYPE slis_selfield.

  DATA ref1 TYPE REF TO cl_gui_alv_grid.
  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
    IMPORTING
      e_grid = ref1.
  CALL METHOD ref1->check_changed_data.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_check_coupa
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> VG_VALIDO
*&---------------------------------------------------------------------*
FORM f_check_coupa  USING p_contrato p_material CHANGING   p_vg_valido.
  DATA: lt_retorno      TYPE TABLE OF zmme_supp_itens,
        lv_string       TYPE string,
        lv_data_conv    TYPE string,
        lv_datum        TYPE timestamp,
        lv_info_request TYPE string,
        lv_offset       TYPE string,
        lv_matnr        TYPE matnr,
        lv_matnr18      TYPE matnr18,
        c_numeric       TYPE string VALUE ' -.0123456789'.

  SELECT COUNT(*)
        FROM tvarvc
        WHERE name EQ 'COUPA_CHECK_CONTRATO'
        AND   low = 'X'.
  IF sy-subrc IS INITIAL .
    p_vg_valido = 'X'.
    EXIT.
  ENDIF.

  DO.
    lv_info_request = p_contrato && ';' && p_material && ';' && lv_offset.
    lv_offset = lv_offset + 50.
    zcl_int_ob_supplier_item_contr=>zif_integracao_outbound~get_instance( )->execute_request( EXPORTING i_info_request = lv_info_request IMPORTING e_integracao = DATA(r_response) ).
    IF r_response IS NOT INITIAL.
      lv_string = r_response-ds_data_retorno.
      REPLACE ALL OCCURRENCES OF '-' IN lv_string WITH '_'.
      /ui2/cl_json=>deserialize( EXPORTING json = lv_string CHANGING data = lt_retorno ).
      IF lt_retorno IS INITIAL.
        CLEAR lv_offset.
        EXIT.
      ENDIF.

      LOOP AT lt_retorno ASSIGNING FIELD-SYMBOL(<fs_retorno>).
        IF <fs_retorno>-price CO c_numeric.
          p_vg_valido = 'X'.
          EXIT.
        ENDIF.

      ENDLOOP.
      IF p_vg_valido = 'X'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDDO.

ENDFORM.

*** Stefanini - IR259669 - 25/09/2025 - LAZAROSR - Início de Alteração
FORM f_subtrair_saldo USING i_s_ekpo           TYPE ekpo
                      CHANGING c_v_saldo_pedid TYPE ty_agrup-saldo_pedid.

  TRY.


      READ TABLE t_ekbe TRANSPORTING NO FIELDS
                                      WITH KEY ebeln = i_s_ekpo-ebeln
                                               ebelp = i_s_ekpo-ebelp
                                                         BINARY SEARCH.

      IF sy-subrc IS INITIAL.

        LOOP AT t_ekbe INTO DATA(ls_ekbe) FROM sy-tabix.

          IF ls_ekbe-ebeln NE i_s_ekpo-ebeln
          OR ls_ekbe-ebelp NE i_s_ekpo-ebelp.
            EXIT.
          ENDIF.

          c_v_saldo_pedid = c_v_saldo_pedid - ls_ekbe-menge.

        ENDLOOP.

      ENDIF.

    CATCH cx_root.
      " Ocorreu algum erro ao efetuar o cálculo...
  ENDTRY.

ENDFORM.
*** Stefanini - IR259669 - 25/09/2025 - LAZAROSR - Fim de Alteração
