FUNCTION zpm_importa_nota_pm.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(NOTAS) TYPE  ZPME0015_T
*"     REFERENCE(ONLINE) TYPE  CHAR1
*"  EXPORTING
*"     REFERENCE(RETORNO) TYPE  ZTPM_RETORNO_NOTA
*"----------------------------------------------------------------------

  DATA: t_entrada             TYPE TABLE OF zpmt0014,
        t_causas              TYPE TABLE OF zpmt0019,
        wa_notifheader_export TYPE bapi2080_nothdre,
        t_notifcaus           TYPE TABLE OF bapi2080_notcausi,
        t_notitem             TYPE TABLE OF bapi2080_notitemi,
        t_notifactv           TYPE TABLE OF bapi2080_notactvi,
        it_return             TYPE TABLE OF bapiret2,
        lt_itens_nota         TYPE ztpm_itens.

  DATA  t_zpmt0019 TYPE zpmt0019_t.
  DATA: t_zpmt0014  TYPE zpmt0014_t,
        lt_item     TYPE TABLE OF bapi2080_notiteme,
        lt_caus     TYPE TABLE OF bapi2080_notcause,
        lt_fact     TYPE TABLE OF bapi2080_notactve,
        lt_return   TYPE TABLE OF bapiret2,
        lt_zpmt0077 TYPE TABLE OF zpmt0077,
        lv_seq      TYPE sy-tabix.

  DATA: id_nota TYPE char10.
  DATA: id_causa    TYPE char10,
        lv_difdate  TYPE p,
        lv_diftime  TYPE p,
        lv_dtinitav TYPE datum,
        lv_hrinitav TYPE uzeit,
        lv_dtfimtav TYPE datum,
        lv_hrfimtav TYPE uzeit.

  DATA(obj_create) = NEW zcl_pm_ordem( ).

  LOOP AT notas INTO DATA(_wa).

    id_nota = obj_create->get_idnot( ).



    APPEND VALUE #(
      idnot = |{ id_nota ALPHA = IN }|
      iwerk = _wa-iwerk
      aufnr = |{ _wa-aufnr ALPHA = IN }|
      qmnum = |{ _wa-qmnum ALPHA = IN }|
      tplnr = _wa-tplnr
      equnr = _wa-equnr
      arbpl = _wa-arbpl
      arjid = _wa-arjid
      ausvn = obj_create->convert_date( i_data = CONV #( _wa-ausvn ) i_versao = abap_true )
      ausbs = obj_create->convert_date( i_data = CONV #( _wa-ausbs ) i_versao = abap_true )
      auztv = obj_create->convert_time( _wa-auztv )
      auztb = obj_create->convert_time( _wa-auztb )
      auszt = lv_diftime
      maueh = _wa-maueh
      btpln = _wa-btpln
      bequi = _wa-bequi
      qmart = _wa-qmart
      qmtxt = _wa-qmtxt
      txtnt	= _wa-txtnt
      artpr = _wa-artpr
      priok = _wa-priok
      objnr = _wa-objnr
      msaus = _wa-msaus
      istat = _wa-istat
      txt04 = _wa-txt04
      qmgrp = _wa-qmgrp
      qmcod = _wa-qmcod
      ernam = sy-uname
      dtreg = sy-datum
      hrreg = sy-uzeit
      statp = _wa-statp
*** Inicio- Rubenilson Pereira - 28.07.2022 - US83600 - MOBMAN
      ingrp = _wa-ingrp
      strmn = obj_create->convert_date( i_data = CONV #( _wa-strmn ) i_versao = abap_true )
      strur = obj_create->convert_time( _wa-strur )
      ltrmn = obj_create->convert_date( i_data = CONV #( _wa-ltrmn ) i_versao = abap_true )
      ltrur = obj_create->convert_time( _wa-ltrur )
      qmnam = _wa-qmnam
*** Fim- Rubenilson Pereira - 28.07.2022 - US83600 - MOBMAN
    ) TO t_entrada.

    LOOP AT _wa-part_objnr INTO DATA(_wa1).

      id_causa = sy-tabix.

      APPEND VALUE #(
                      idcau = |{ id_causa ALPHA = IN }|
                      idnot = |{ id_nota  ALPHA = IN }|
                      fenum = _wa1-numero_item
                      otkat = _wa1-otkat
                      otgrp = _wa1-otgrp
                      oteil = _wa1-oteil
                      fekat = _wa1-fekat
                      fegrp = _wa1-fegrp
                      fecod = _wa1-fecod
                      fetxt = _wa1-fetxt
                      urkat = _wa1-urkat
                      urgrp = _wa1-urgrp
                      urcod = _wa1-urcod
                      urstx = _wa1-urstx
                      mnkat = _wa1-mnkat
                      mngrp = _wa1-mngrp
                      mncod = _wa1-mncod
                      eliminado = _wa1-eliminado
                    ) TO t_causas.

    ENDLOOP.
  ENDLOOP.

  MODIFY zpmt0014 FROM TABLE t_entrada.
  MODIFY zpmt0019 FROM TABLE t_causas.
  COMMIT WORK.

*   "// Pega a quantidade de linhas enviada pelo Mobile
  DATA(qtd_item) = lines( notas ).

  IF online IS INITIAL.
    retorno-sucess = abap_true.
    retorno-data =
    VALUE #(

              msg_nota = COND #( WHEN qtd_item EQ 1
                                                    THEN |Foi armazenado { qtd_item } item com sucesso!|
                                                    ELSE |Foram armazenados { qtd_item } itens com sucesso!| )
            ).
*   "// Processa os Dados que foi Enviado pelo Mobile
    CALL METHOD obj_create->call_report
      EXPORTING
        i_sequen = CONV #( |{ id_nota ALPHA = OUT }| )
        i_report = 'ZPMR0046'.
*   "// Finaliza o processo em caso de dados OffLine
    EXIT.
  ENDIF.

* "// Se OnLine continua o processo
* "// seleciona os dados da Nota pegando somente os que foi enviado no momento pelo Mobile
  SELECT *
    FROM zpmt0014
    INTO TABLE t_zpmt0014
    WHERE statp EQ abap_false.

  CHECK sy-subrc IS INITIAL.

  SELECT *
    FROM zpmt0019
    INTO TABLE t_zpmt0019
    FOR ALL ENTRIES IN t_zpmt0014
    WHERE idnot EQ t_zpmt0014-idnot.

  LOOP AT t_zpmt0014 INTO DATA(w_notas).

    CALL METHOD obj_create->criar_notas
      EXPORTING
        i_online = online
        i_notas  = w_notas       " Estrutura Notas
        t_causas = t_zpmt0019    " Tabela de causas
      IMPORTING
        e_nota   = DATA(_nota)   " Retorno da Nota
        e_msg    = DATA(_msg).   " Retorno da mensagen de Erro/Sucesso

    IF _nota IS NOT INITIAL.

*     "// Preenche a Nota com ZEROS
      w_notas-qmnum = |{ _nota ALPHA = IN }|.
*** Inicio- Rubenilson Pereira - 28.07.2022 - US83600 - MOBMAN
      SELECT SINGLE *
        FROM qmih
        INTO @DATA(ls_qmih)
        WHERE qmnum = @w_notas-qmnum.
      IF sy-subrc IS INITIAL.

        lv_dtinitav = ls_qmih-ausvn.
        lv_hrinitav = ls_qmih-auztv.
        lv_dtfimtav = ls_qmih-ausbs.
        lv_hrfimtav = ls_qmih-auztb.

        CALL FUNCTION 'SD_DATETIME_DIFFERENCE'
          EXPORTING
            date1            = lv_dtinitav
            time1            = lv_hrinitav
            date2            = lv_dtfimtav
            time2            = lv_hrfimtav
          IMPORTING
            datediff         = lv_difdate
            timediff         = lv_diftime
          EXCEPTIONS
            invalid_datetime = 1
            OTHERS           = 2.
        IF sy-subrc = 0.

          lv_difdate = lv_difdate * 24.

          lv_diftime = lv_diftime + lv_difdate.

          ls_qmih-auszt = lv_diftime * 3600.

          CALL FUNCTION 'ISU_DB_QMIH_UPDATE'
            EXPORTING
              x_qmih     = ls_qmih
              x_upd_mode = 'U'.

          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

        ENDIF.
      ENDIF.
*** Fim- Rubenilson Pereira - 28.07.2022 - US83600 - MOBMAN

*     "// Autera o Status do Processamento para "P" de Processado
      w_notas-statp = 'P'.  "// Processado
*     "// Aplica na Tabela Fisica
      MODIFY zpmt0014 FROM w_notas.

*     "// retorna a mensagem de Sucesso
      retorno-sucess = abap_true.
      retorno-data =
       VALUE #(
                nr_nota  = w_notas-qmnum
                msg_nota = _msg
              ).

      WAIT UP TO 1 SECONDS.
      ADD 1 TO lv_seq.
      APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING FIELD-SYMBOL(<fs_zpmt0077>).
      <fs_zpmt0077>-aufnr = w_notas-qmnum.
      <fs_zpmt0077>-seq   = lv_seq.
      <fs_zpmt0077>-data = sy-datum.
      <fs_zpmt0077>-hora = sy-uzeit.
      <fs_zpmt0077>-tp_operacao = 'NOTA'.
      <fs_zpmt0077>-observacao =  'Sucesso na criação da nota'.

      CALL FUNCTION 'BAPI_ALM_NOTIF_GET_DETAIL'
        EXPORTING
          number             = w_notas-qmnum
        IMPORTING
          notifheader_export = wa_notifheader_export
        TABLES
          notitem            = lt_item
          notifcaus          = lt_caus
          notifactv          = lt_fact
          return             = lt_return.
      IF sy-subrc IS INITIAL.

        IF wa_notifheader_export-notif_type EQ 'Y3'.

          REFRESH lt_itens_nota.

          LOOP AT lt_fact ASSIGNING FIELD-SYMBOL(<fs_fact>).

            APPEND INITIAL LINE TO lt_itens_nota ASSIGNING FIELD-SYMBOL(<fs_itens_nota>).
            <fs_itens_nota>-numero = <fs_fact>-act_key.

          ENDLOOP.

          retorno-data-itens = lt_itens_nota.

        ELSE.

          REFRESH lt_itens_nota.

          LOOP AT lt_item ASSIGNING FIELD-SYMBOL(<fs_item>).
            APPEND INITIAL LINE TO lt_itens_nota ASSIGNING <fs_itens_nota>.
            <fs_itens_nota>-numero = <fs_item>-item_key.
          ENDLOOP.

          retorno-data-itens = lt_itens_nota.

        ENDIF.
      ENDIF.

    ELSE.

      IF _msg IS INITIAL.
        ADD 1 TO lv_seq.
        APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
        <fs_zpmt0077>-aufnr = w_notas-qmnum.
        <fs_zpmt0077>-seq   = lv_seq.
        <fs_zpmt0077>-data = sy-datum.
        <fs_zpmt0077>-hora = sy-uzeit.
        <fs_zpmt0077>-tp_operacao = 'NOTA'.
        <fs_zpmt0077>-observacao =  'Erro na criação/modificação da nota, variavel _msg vazia'.
      ENDIF.


*     "// Autera o Status do Processamento para "E" de Erro
      w_notas-statp = 'E'.  "// Erro no Processamento
*     "// Aplica na Tebela Fisica
      MODIFY zpmt0014 FROM w_notas.
*     "// retorna a mensagem de Erro
      retorno-erros =
       VALUE #(
                msg_erros = _msg
              ).
    ENDIF.
  ENDLOOP.

  IF lt_zpmt0077 IS NOT INITIAL.
    MODIFY zpmt0077 FROM TABLE lt_zpmt0077.
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
    ENDIF.
  ENDIF.
ENDFUNCTION.
