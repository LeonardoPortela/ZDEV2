*&---------------------------------------------------------------------*
*& Report  ZMMR0030
*&--------------------------------------------------------------------&*
*& Projeto..: Amaggi                                                  &*
*& Autor....: Izyan Nascimento                                        &*
*& Data.....: 14/04/2015                                              &*
*& Descrição: Interface TRACECOTTON x SAP PP - IN                     &*
*& Transação: PP                                                      &*
*& Request..: DEVK945561                                              &*
*&--------------------------------------------------------------------&*
REPORT  zmmr0030  MESSAGE-ID ztracecotton.
TYPE-POOLS vrm.

** CONSTANTS
**----------------------------------------------------------------------
CONSTANTS: c_x TYPE c VALUE 'X',
           c_p TYPE c VALUE 'P'.

** TYPES
**----------------------------------------------------------------------
TYPES: BEGIN OF ty_saida,
         mark,
         row              TYPE i,
         budat            TYPE zppt0002-budat,
         bldat            TYPE zppt0002-bldat,
         id_fardinho      TYPE zppt0002-id_fardinho,             "Id_fardinho
         normt            TYPE mara-normt,                       "Tipo material          1
         matnr            TYPE mara-matnr,                       "Material               2
         menge            TYPE zppt0002-menge,                   "Quantidade             3
         meins            TYPE mara-meins,                       "UM                     4
         werks            TYPE zppt0002-werks,                   "Centro                 5
         verid            TYPE zppt0002-verid,                   "Versao(maquina)        6
         n_verid          TYPE mkal-text1,                       "Nome da versão         7
         acharg           TYPE zppt0002-acharg,                  "Lote                   8
         alort            TYPE stpo-lgort,                       "Deposito               9
         matnr_con        TYPE zppt0002-matnr,                   "Material consumo       10
         menge_con        TYPE zppt0002-menge,                   "Quantidade consumo     11
         meins_con        TYPE mara-meins,                       "UM consumo             12
         lgort_con        TYPE stpo-lgort,                       "Deposito consumo       13
         charg_con        TYPE zppt0002-charg,                   "Fardao Origem          14
         id_fardao        TYPE zppt0002-id_fardao,               "Fardao Origem          14
         budat_con        TYPE zppt0002-budat,                   "Data Lançamento        15
         cd_classificacao TYPE zppt0002-cd_classificacao,        "Classificação  16
         status_con       TYPE zppt0002-status,                  "Status                 17
         mblnr            TYPE zppt0002-mblnr,                   "N° Documento material  18
         cd_safra         TYPE zppt0002-cd_safra,
         id_cotton        TYPE zppt0002-id_cotton,
       END OF ty_saida,

       BEGIN OF type_mchb,
         matnr           TYPE mchb-matnr,
         werks           TYPE mchb-werks,
         lgort           TYPE mchb-lgort,
         charg           TYPE mchb-charg,
         clabs           TYPE mchb-clabs,
         cspem           TYPE mchb-cspem,
         maktx           TYPE makt-maktx,
         lgortr          TYPE mchb-lgort,
         chargr          TYPE mchb-charg,
         matnc           TYPE mchb-matnr,
         status_registro TYPE zppt0002-status_registro,
       END   OF type_mchb,

       BEGIN OF ty_mchb,
         matnr TYPE mchb-matnr,
         werks TYPE mchb-werks,
         lgort TYPE mchb-lgort,
         charg TYPE mchb-charg,
         normt TYPE mara-normt,
       END   OF ty_mchb,

       BEGIN OF ty_mseg,
         charg TYPE mseg-charg,
         mblnr TYPE mseg-mblnr,
         smbln TYPE mseg-smbln,
       END   OF ty_mseg.

DATA: l_plaf         TYPE TABLE OF  plaf WITH HEADER LINE,
      mt61d          LIKE  mt61d,
      l_cm61m        LIKE  cm61m,
      eselid         LIKE  af61z-selid,
      xscrap         TYPE  xfeld,
      esbflg         LIKE  cm61x-sbflg,
      mdpmx	         TYPE TABLE OF mdpm WITH HEADER LINE,
      vsaldo         TYPE mchb-clabs,
      sperr_user     TYPE sy-msgv1,
      fg_bloqueio(1),
      vlines         TYPE sy-tabix.


** INTERNAL TABLES
**----------------------------------------------------------------------
DATA: it_zppt0002     TYPE TABLE OF zppt0002,
      it_zppt0002_tot TYPE TABLE OF zppt0002,
      it_zppt0002_far TYPE TABLE OF zppt0002,
      it_zppt0002_tra TYPE TABLE OF zppt0002,
      it_zppt0002_est TYPE TABLE OF zppt0002,
      t_trans         TYPE TABLE OF type_mchb,
      it_saida        TYPE TABLE OF ty_saida,
      it_mseg_tot     TYPE TABLE OF ty_mseg,
      it_zppt0006     TYPE TABLE OF zppt0006.

** WORK AREAS
**----------------------------------------------------------------------
DATA: wa_zppt0002     TYPE zppt0002,
      wa_zppt0002_tot TYPE zppt0002,
      wa_saida        TYPE ty_saida,
      wa_zppt0006     TYPE zppt0006,
      wa_mara         TYPE mara,
      sl_trans        TYPE type_mchb,
      wa_mseg_tot     TYPE ty_mseg,
      v_objek         TYPE inob-objek.


** VARIABLES
**----------------------------------------------------------------------
DATA:
      vg_cont         TYPE i.


** VARIABLES BABI
DATA: es_bflushflags   LIKE bapi_rm_flg,
      es_bflushdatagen LIKE bapi_rm_datgen,
      es_confirmation  LIKE bapi_rm_datkey-confirmation,
      gs_blpp          LIKE blpp,
      it_return        TYPE TABLE OF bapiret2,
      wa_return        TYPE bapiret2,
      it_outreturn     TYPE TABLE OF zfie_ret_document,
      wa_outreturn     TYPE zfie_ret_document,
      vg_obj_key       TYPE zmmt_ee_zgr-obj_key,
      vid              TYPE zppt0006-id,
      vg_interface(2).

DATA: it_goodsmovements  TYPE TABLE OF bapi2017_gm_item_create,
      wa_goodsmovements  TYPE bapi2017_gm_item_create,
      it_bapi_char_batch TYPE TABLE OF bapi_char_batch,
      wa_bapi_char_batch TYPE bapi_char_batch,
      es_goodsmvt_header LIKE bapi2017_gm_head_01,
      es_goodsmvt_code   LIKE bapi2017_gm_code.

DATA: vobjecttable   TYPE bapi1003_key-objecttable,
      vclassnum      TYPE bapi1003_key-classnum,
      vclasstype     TYPE bapi1003_key-classtype,
      it_allocvalues TYPE TABLE OF  bapi1003_alloc_values_char,
      wa_allocvalues TYPE bapi1003_alloc_values_char,
      it_allocnum    TYPE TABLE OF  bapi1003_alloc_values_num,
      it_alloccur    TYPE TABLE OF  bapi1003_alloc_values_curr,
      it_return2     TYPE TABLE OF bapiret2.

DATA: it_mseg1 TYPE TABLE OF ty_mseg,
      it_mchb  TYPE TABLE OF ty_mchb,
      it_mchb2 TYPE TABLE OF mchb,
      wa_mchb  TYPE ty_mchb,
      wa_mchb2 TYPE mchb,
      wa_mseg1 TYPE ty_mseg.

*&---------------------------------------------------------------------*
*&      Start-Of-Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  DATA: vg_job      TYPE i.

  SELECT SINGLE COUNT( * ) INTO vg_job
    FROM tbtco
   WHERE jobname EQ 'TRACECOTON'
     AND status EQ 'R'.


  vg_interface = '39'.
  IF ( vg_job EQ 1 ).
    PERFORM f_seleciona_dados.
    PERFORM f_fardao.
    PERFORM f_fardinho.
    PERFORM f_transferencia.
    PERFORM f_gravar_dados.
    "
*    IF NOT IT_OUTRETURN[] IS INITIAL.
*      SORT IT_OUTRETURN BY OBJ_KEY INTERFACE.
*
*      CALL FUNCTION 'Z_FI_OUTBOUND_RETURN' IN BACKGROUND TASK
*        DESTINATION 'XI_SIGAM_RETURN'
*        TABLES
*          OUTRETURN = IT_OUTRETURN.
*    ENDIF.
*    COMMIT WORK.
    "
    PERFORM f_envia_trace.

  ENDIF.


*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_seleciona_dados.

  REFRESH: it_zppt0002, it_saida.

  " Lotes sem classificação (totaliza fardão)
  SELECT * FROM zppt0002
    INTO TABLE it_zppt0002_far
  WHERE status_registro  EQ '01'
  AND   status           EQ 'R'.

  SORT it_zppt0002_far BY acharg.
  DELETE ADJACENT DUPLICATES FROM it_zppt0002_far COMPARING acharg.

  " Lotes com classificação
  SELECT * FROM zppt0002
    INTO TABLE it_zppt0002
  WHERE status_registro  EQ '03'
  AND   dt_classificacao NE ''
  AND   status           EQ 'R'.

  SORT it_zppt0002 BY acharg.
  DELETE ADJACENT DUPLICATES FROM it_zppt0002 COMPARING acharg.

  IF it_zppt0002 IS NOT INITIAL.

* verifica duplicata e estorna
    it_zppt0002_est = it_zppt0002.

    SORT it_zppt0002_est BY id_cotton.
    DELETE ADJACENT DUPLICATES FROM it_zppt0002_est COMPARING id_cotton.

    LOOP AT it_zppt0002_est INTO DATA(wa_zppt0002_est).
      zcl_integracao_cotton_sap=>estorno_04( wa_zppt0002_est-id_cotton ).
    ENDLOOP.
* Verifica Duplicata e estorna

    LOOP AT it_zppt0002 INTO wa_zppt0002.

      wa_mchb-charg = wa_zppt0002-acharg.
      wa_mchb-lgort = wa_zppt0002-lgort.
      wa_mchb-matnr = wa_zppt0002-matnr.
      wa_mchb-werks = wa_zppt0002-werks.
      wa_mchb-normt = wa_zppt0002-cd_classificacao.

      APPEND wa_mchb TO it_mchb.

    ENDLOOP.

    SELECT *
      FROM mara
      INTO TABLE @DATA(it_mara2)
      FOR ALL ENTRIES IN @it_mchb
      WHERE normt EQ @it_mchb-normt
        AND mtart EQ 'ZFER'
        AND mstae NE '02'.

    LOOP AT it_mchb ASSIGNING FIELD-SYMBOL(<f_mchb>).
      READ TABLE it_mara2 INTO DATA(wa_mara) WITH KEY normt = <f_mchb>-normt.
      IF sy-subrc IS INITIAL.
        <f_mchb>-matnr = wa_mara-matnr.
      ENDIF.
    ENDLOOP.

    IF it_mchb IS NOT INITIAL.
      SELECT *
        FROM mchb
        INTO TABLE it_mchb2
        FOR ALL ENTRIES IN it_mchb
        WHERE charg EQ it_mchb-charg
          AND lgort EQ it_mchb-lgort
          AND matnr EQ it_mchb-matnr
          AND werks EQ it_mchb-werks.

      SORT it_mchb2 BY charg werks.

      IF it_mchb2 IS NOT INITIAL.

        SELECT *
          FROM mseg
          INTO CORRESPONDING FIELDS OF TABLE it_mseg1
          FOR ALL ENTRIES IN it_mchb2
          WHERE charg = it_mchb2-charg
            AND werks = it_mchb2-werks
            AND matnr = it_mchb2-matnr
            AND lgort = it_mchb2-lgort
            AND bwart IN ('131', '132').

        LOOP AT it_mseg1 ASSIGNING FIELD-SYMBOL(<f_mseg1>).
          READ TABLE it_mseg1 INTO wa_mseg1 WITH KEY charg = <f_mseg1>-charg smbln = <f_mseg1>-mblnr.
          IF sy-subrc IS INITIAL.
            <f_mseg1>-smbln = wa_mseg1-mblnr.
          ENDIF.
        ENDLOOP.

        DELETE it_mseg1 WHERE smbln IS NOT INITIAL.

        SORT it_mseg1 BY charg.

      ENDIF.
    ENDIF.
  ENDIF.

  " transferencia de bloco 07 /08 SUCESS0 / transferencia de tipo 09 10 SUCESS0 E10
  SELECT * FROM zppt0002
    INTO TABLE it_zppt0002_tra
  WHERE ( status_registro  EQ '07' "TRANSFERE DEPOSITO
  OR    status_registro    EQ '09' )  "TRANSFERE DE MATERIAL
  AND   status           EQ 'R'.

  SORT it_zppt0002_tra BY acharg.
  DELETE ADJACENT DUPLICATES FROM it_zppt0002_tra COMPARING acharg.

ENDFORM.                    " F_SELECIONA_DADOS

*&---------------------------------------------------------------------*
*&      Form  F_FARDAO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_fardao.
  DATA: wa_ztsafrafardos    TYPE ztsafrafardos,
        vg_qtd_fardinhos    TYPE zppt0002-qtd_fardinhos,
        st_qtd_fardinhos(5),
        st_qtd_fardao(5),
        v_confirmation_es   TYPE bapi_rm_datkey-cancconfirmation,
        wa_mkal             TYPE mkal.



  CHECK it_zppt0002_far IS NOT INITIAL.

  SELECT *
    FROM zpps_ximfbf_log
    INTO TABLE @DATA(it_zpps_ximfbf_log)
    FOR ALL ENTRIES IN @it_zppt0002_far
    WHERE id_cotton = @it_zppt0002_far-id_cotton
    AND NOT EXISTS ( SELECT * FROM mseg WHERE mseg~smbln = zpps_ximfbf_log~mblnr  ).

  SELECT *
    FROM zmmt0006 AS m1
    INTO TABLE @DATA(it_zmmt0006)
    FOR ALL ENTRIES IN @it_zppt0002_far
    WHERE id_cotton = @it_zppt0002_far-id_cotton
    AND  ch_referencia = ( SELECT MAX( m2~ch_referencia ) FROM zmmt0006 AS m2 WHERE m2~id_cotton = m1~id_cotton ) .

  DELETE it_zmmt0006 WHERE tcode  = 'MBST'.

  SORT it_zmmt0006 BY id_cotton.
  SORT it_zpps_ximfbf_log BY id_cotton.

  LOOP AT it_zppt0002_far INTO wa_zppt0002.
    DATA(tabix) = sy-tabix.

    IF wa_zppt0002-id_cotton IS NOT  INITIAL.
      READ TABLE it_zpps_ximfbf_log INTO DATA(wa_zpps_ximfbf_log) WITH KEY id_cotton = wa_zppt0002-id_cotton BINARY SEARCH.
      IF sy-subrc = 0.
        wa_zppt0002-charg = wa_zpps_ximfbf_log-charg.
        MODIFY it_zppt0002_far FROM wa_zppt0002 INDEX tabix TRANSPORTING charg.
      ELSE.
        READ TABLE it_zmmt0006 INTO DATA(wa_zmmt0006)  WITH KEY id_cotton = wa_zppt0002-id_cotton BINARY SEARCH.
        IF sy-subrc = 0.
          wa_zppt0002-charg = wa_zmmt0006-batch_d.
          MODIFY it_zppt0002_far FROM wa_zppt0002 INDEX tabix TRANSPORTING charg.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  MODIFY zppt0002 FROM TABLE it_zppt0002_far.
  COMMIT WORK.

  SELECT *
    FROM mseg
     INTO CORRESPONDING FIELDS OF TABLE it_mseg_tot
      FOR ALL ENTRIES IN it_zppt0002_far
    WHERE charg = it_zppt0002_far-charg
      AND werks = it_zppt0002_far-werks
      AND matnr = '000000000000119952'
      AND bwart IN ('261', '262').

  LOOP AT it_mseg_tot ASSIGNING FIELD-SYMBOL(<f_mseg_tot>).
    READ TABLE it_mseg_tot INTO wa_mseg_tot WITH KEY charg = <f_mseg_tot>-charg smbln = <f_mseg_tot>-mblnr.
    IF sy-subrc IS INITIAL.
      <f_mseg_tot>-smbln = wa_mseg_tot-mblnr.
    ENDIF.
  ENDLOOP.

  DELETE it_mseg_tot WHERE smbln IS NOT INITIAL.
  SORT it_mseg_tot BY charg.


  SORT it_zppt0002_far BY werks charg.

  it_zppt0002_tot[] = it_zppt0002_far[].

  DELETE ADJACENT DUPLICATES FROM it_zppt0002_tot COMPARING werks charg.

  LOOP AT it_zppt0002_tot INTO wa_zppt0002_tot.

    wa_zppt0002_tot-menge        = 0.
    wa_zppt0002_tot-peso_caroco  = 0.
    wa_zppt0002_tot-peso_fibrilha = 0.
    vg_qtd_fardinhos      = 0.
    REFRESH: it_return.
    "
    "checa se encontrou o lote de produção / transferencia
    IF wa_zppt0002_tot-charg IS INITIAL.
      wa_return-type    = 'E'.
      wa_return-number  = 1.
      CONCATENATE 'Não existe entrada de prod. p/ o rolo'   wa_zppt0002_tot-id_cotton INTO wa_return-message SEPARATED BY space.
      APPEND wa_return TO it_return.
    ENDIF.
    IF it_return[] IS NOT INITIAL.
      PERFORM: f_monta_msg_fd.
      CONTINUE.
    ENDIF.
    "
    LOOP AT it_zppt0002_far INTO wa_zppt0002 WHERE werks = wa_zppt0002_tot-werks
                                             AND   charg = wa_zppt0002_tot-charg.

      ADD wa_zppt0002-menge         TO wa_zppt0002_tot-menge.
      ADD wa_zppt0002-peso_caroco   TO wa_zppt0002_tot-peso_caroco.
      ADD wa_zppt0002-peso_fibrilha TO wa_zppt0002_tot-peso_fibrilha.
      ADD 1 TO vg_qtd_fardinhos.
    ENDLOOP.


    SELECT SINGLE cslid
      INTO eselid
      FROM t399d
      WHERE werks = wa_zppt0002_tot-werks.

    REFRESH: mdpmx, l_plaf.
    l_plaf-matnr = wa_zppt0002_tot-matnr.
    l_plaf-plwrk = wa_zppt0002_tot-werks.
    l_plaf-pwwrk = wa_zppt0002_tot-werks.
    l_plaf-gsmng = wa_zppt0002_tot-menge.
    l_plaf-psttr = wa_zppt0002_tot-budat.
    l_plaf-verid = wa_zppt0002_tot-verid.
    APPEND l_plaf.

    CALL FUNCTION 'MD_AUFLOESUNG_PLANAUFTRAG'
      EXPORTING
        eplaf         = l_plaf   " Planauftrag
        emt61d        = mt61d    " Matstammview
        eselid        = eselid          " SelektionsID Stüli/ DispWerkTab
        ecm61m        = l_cm61m  " CO-Bereich Materialartenebene
        eno_scrap     = xscrap "VGA
      IMPORTING
        iplaf         = l_plaf
      TABLES
        mdpmx         = mdpmx    " View auf Matkomponenten im Pauf
      EXCEPTIONS
        error_message = 1.

    CLEAR: es_bflushflags, es_bflushdatagen, wa_return.
    REFRESH: it_return, it_goodsmovements.

    "checa a quantidade de fardinhos do lote
    IF vg_qtd_fardinhos NE wa_zppt0002_tot-qtd_fardinhos.
      wa_return-type    = 'E'.
      wa_return-number  = 1.
      st_qtd_fardinhos  = vg_qtd_fardinhos.
      st_qtd_fardao     = wa_zppt0002_tot-qtd_fardinhos.
      CONCATENATE 'Enviados  ' st_qtd_fardinhos ' fardinhos de ' st_qtd_fardao ' Lote'   wa_zppt0002_tot-charg INTO wa_return-message SEPARATED BY space.
      APPEND wa_return TO it_return.
    ENDIF.

    IF it_return[] IS NOT INITIAL.
      PERFORM: f_monta_msg_fd.
      CONTINUE.
    ENDIF.

    LOOP AT mdpmx.
      CLEAR: vsaldo.
      IF mdpmx-shkzg = 'H'. "somente saida

        READ TABLE it_mseg_tot INTO wa_mseg_tot WITH KEY charg  = wa_zppt0002_tot-charg.
        IF sy-subrc IS INITIAL.
          UPDATE zppt0002 SET status_registro = '02'
                              mblnr02         = wa_mseg_tot-mblnr
                       WHERE charg = wa_zppt0002_tot-charg
                         AND werks = wa_zppt0002_tot-werks
                         AND status_registro = '01'.

          wa_return-type       = 'S'.
          CONCATENATE 'Documento de Fardão gerado:' wa_mseg_tot-mblnr INTO wa_return-message.
          wa_return-message_v1 = '02'.
          wa_return-message_v2 = wa_mseg_tot-mblnr.
          APPEND wa_return TO it_return.
        ENDIF.

        IF it_return[] IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        IF mdpmx-xchpf = 'X'. "lote obrigatório
          SELECT SINGLE clabs
            FROM mchb
            INTO vsaldo
            WHERE matnr = mdpmx-matnr
            AND   werks = wa_zppt0002_tot-werks
            AND   lgort = mdpmx-lgpro
            AND   charg = wa_zppt0002_tot-charg.
        ELSE.
          SELECT SINGLE labst
            FROM mard
            INTO vsaldo
            WHERE matnr = mdpmx-matnr
            AND   werks = wa_zppt0002_tot-werks
            AND   lgort = mdpmx-lgpro.
        ENDIF.

*        120166 - FIBRILHA
*        120168 - CAROÇO ALGODAO PROD PROPRIA
        IF mdpmx-matnr = '000000000000120166'.
          IF wa_zppt0002_tot-peso_fibrilha GT 0.
            mdpmx-erfmg = wa_zppt0002_tot-peso_fibrilha.
          ENDIF.
        ELSEIF mdpmx-matnr = '000000000000120168'.
          IF wa_zppt0002_tot-peso_caroco GT 0.
            mdpmx-erfmg = wa_zppt0002_tot-peso_caroco.
          ENDIF.
        ENDIF.

        IF ( mdpmx-erfmg GT vsaldo AND mdpmx-lgpro+0(3) NE 'ARM' ) OR ( vsaldo LE 0 AND mdpmx-lgpro+0(3) EQ 'ARM' ).
          wa_return-type    = 'E'.
          wa_return-number  = 2.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = mdpmx-matnr
            IMPORTING
              output = mdpmx-matnr.
          CONCATENATE 'Falta estoque ' mdpmx-matnr 'Dep.' mdpmx-lgpro 'Lote' wa_zppt0002_tot-charg INTO wa_return-message SEPARATED BY space.
          APPEND wa_return TO it_return.
        ENDIF.


      ENDIF.
    ENDLOOP.

    IF it_return[] IS NOT INITIAL.
      PERFORM: f_monta_msg_fd.
      CONTINUE.
    ENDIF.

    SELECT SINGLE * FROM mkal INTO wa_mkal WHERE werks = wa_zppt0002_tot-werks
                                               AND   matnr = wa_zppt0002_tot-matnr
                                               AND   verid = wa_zppt0002_tot-verid.
    es_bflushflags-bckfltype        = '01'.

    es_bflushdatagen-postdate       = wa_zppt0002_tot-budat.
    es_bflushdatagen-docdate        = wa_zppt0002_tot-bldat.
    es_bflushdatagen-prodplant      = wa_zppt0002_tot-werks.      "'Centro (WERKS)'

* > 05/07/2023 - Migração S4 - LM
*    es_bflushdatagen-materialnr     = wa_zppt0002_tot-matnr.      "'Material (MATNR)'

    IF STRLEN( wa_zppt0002_tot-matnr ) > 18.
      es_bflushdatagen-materialnr_long = wa_zppt0002_tot-matnr.   "'Material Consumo (MATNR) - LONG
    ELSE.
      es_bflushdatagen-materialnr      = wa_zppt0002_tot-matnr.   "'Material Consumo (MATNR)
    ENDIF.
* > 05/07/2023 - Migração S4 - LM

    es_bflushdatagen-backflquant    = wa_zppt0002_tot-menge.      "'Quantidade (ERFMG)'
    es_bflushdatagen-unitofmeasure  = 'KG'.                       "'UM (ERFME)'
    es_bflushdatagen-prodversion    = wa_zppt0002_tot-verid.      "'Versão (VERID)'
    es_bflushdatagen-batch          = wa_zppt0002_tot-charg.      "'Lote (ACHARG)'
    IF wa_mkal-elpro IS NOT INITIAL.
      es_bflushdatagen-storageloc     = wa_mkal-elpro.              "'Deposito (ALORT)'
    ELSE.
      es_bflushdatagen-storageloc     = wa_mkal-alort.              "'Deposito (ALORT)'
    ENDIF.


    LOOP AT mdpmx.
* > 05/07/2023 - Migração S4 - LM
*      wa_goodsmovements-material      = mdpmx-matnr.             "'Material Consumo (MATNR)'

      IF STRLEN( mdpmx-matnr ) > 18.
        wa_goodsmovements-material_long = mdpmx-matnr.   "'Material Consumo (MATNR) - LONG
      ELSE.
        wa_goodsmovements-material      = mdpmx-matnr.   "'Material Consumo (MATNR)
      ENDIF.
* > 05/07/2023 - Migração S4 - LM

      wa_goodsmovements-plant         = wa_zppt0002_tot-werks.   "'Centro (WERKS)'
      wa_goodsmovements-stge_loc      = mdpmx-lgpro.             "'Deposito Consumo (LGORT)'
      IF mdpmx-xchpf = 'X'. "lote obrigatório
        wa_goodsmovements-batch         = wa_zppt0002_tot-charg.   "'Lote Consumo (CHARG)'
      ENDIF.
      IF mdpmx-shkzg = 'S'.
        wa_goodsmovements-move_type     = '531'.
        wa_goodsmovements-batch = wa_zppt0002_tot-cd_safra. "ALRS 19/07/2016
        IF mdpmx-matnr = '000000000000120166'.
          IF wa_zppt0002_tot-peso_fibrilha GT 0.
            mdpmx-erfmg = wa_zppt0002_tot-peso_fibrilha.
          ENDIF.
        ELSEIF mdpmx-matnr = '000000000000120168'.
          IF wa_zppt0002_tot-peso_caroco GT 0.
            mdpmx-erfmg = wa_zppt0002_tot-peso_caroco.
          ENDIF.
        ENDIF.
      ELSE.
        wa_goodsmovements-move_type     = '261'.
*        IF MDPMX-LGPRO = 'ARMZ'. " Consome todo o saldo deste lote
        vsaldo = 0.
        SELECT SINGLE clabs
           FROM mchb
           INTO vsaldo
           WHERE matnr = mdpmx-matnr
           AND   werks = wa_zppt0002_tot-werks
           AND   lgort = mdpmx-lgpro
           AND   charg = wa_zppt0002_tot-charg.
        mdpmx-erfmg = vsaldo.
        IF vsaldo LE 0.
          wa_return-type    = 'E'.
          wa_return-number  = 2.
          CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
            EXPORTING
              input  = mdpmx-matnr
            IMPORTING
              output = mdpmx-matnr.
          CONCATENATE 'Material Bloqueado ' mdpmx-matnr 'Dep.' mdpmx-lgpro 'Lote' wa_zppt0002_tot-charg INTO wa_return-message SEPARATED BY space.
          APPEND wa_return TO it_return.
        ENDIF.
      ENDIF.

      wa_goodsmovements-entry_qnt     = mdpmx-erfmg.             "'Quantidade consumo (ERFME_R)'

      wa_goodsmovements-entry_uom     = mdpmx-erfme.             "'UM consumo (ERFME)'
      APPEND wa_goodsmovements TO it_goodsmovements.
    ENDLOOP.

*    IF IT_RETURN[] IS NOT INITIAL.
*      PERFORM: F_MONTA_MSG_FD.
*      CONTINUE.
*    ENDIF.

    READ TABLE it_goodsmovements INTO wa_goodsmovements WITH KEY move_type = '261'.
    IF sy-subrc NE 0 .
      wa_return-type    = 'E'.
      wa_return-number  = 2.
      CONCATENATE 'Material sem baixa, verificar Lote' wa_zppt0002_tot-charg INTO wa_return-message SEPARATED BY space.
      APPEND wa_return TO it_return.
    ENDIF.

    DESCRIBE TABLE it_goodsmovements LINES vlines.
    IF vlines NE 3.
      IF sy-subrc NE 0 .
        wa_return-type    = 'E'.
        wa_return-number  = 2.
        CONCATENATE 'Movimento incompleto, verificar Lote' wa_zppt0002_tot-charg INTO wa_return-message SEPARATED BY space.
        APPEND wa_return TO it_return.
      ENDIF.
    ENDIF.
    CLEAR: fg_bloqueio, sy-msgv1, sperr_user.
* ---> S4 Migration - 20/06/2023 - MA
    DATA: lv_material TYPE bapi2017_gm_item_create-MATERIAL_LONG.
* <--- S4 Migration - 20/06/2023 - MA
    LOOP AT it_goodsmovements INTO wa_goodsmovements.
      IF wa_goodsmovements-entry_qnt LE 0.
        wa_return-type    = 'E'.
        wa_return-number  = 2.
        CONCATENATE 'Movimento ' wa_goodsmovements-move_type 'Qtde invalida'  INTO wa_return-message SEPARATED BY space.
        APPEND wa_return TO it_return.
      ENDIF.
      "bloqueio
* ---> S4 Migration - 20/06/2023 - MA
      lv_material = wa_goodsmovements-material.
*      CALL FUNCTION 'ENQUEUE_EMMARCE'
*        EXPORTING
*          matnr          = wa_goodsmovements-material
*          werks          = wa_goodsmovements-plant
*        EXCEPTIONS
*          foreign_lock   = 1
*          system_failure = 2
*          OTHERS         = 3.

      CALL FUNCTION 'ENQUEUE_EMMARCE'
        EXPORTING
          matnr          = lv_material
          werks          = wa_goodsmovements-plant
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
* <--- S4 Migration - 20/06/2023 - MA
      sperr_user     = sy-msgv1.

      IF sperr_user IS NOT INITIAL.
        fg_bloqueio = 'X'.
        wa_return-type    = 'E'.
        wa_return-number  = 2.
        CONCATENATE 'Material ' wa_goodsmovements-material 'bloqueado por ' sperr_user INTO wa_return-message SEPARATED BY space.
        APPEND wa_return TO it_return.
      ENDIF.
      "bloqueio lote
* ---> S4 Migration - 20/06/2023 - MA
      IF wa_goodsmovements-batch IS NOT INITIAL.
*        CALL FUNCTION 'ENQUEUE_EMMCH1E'
*          EXPORTING
*            mode_mch1      = 'E'
*            mandt          = sy-mandt
*            matnr          = wa_goodsmovements-material
*            charg          = wa_goodsmovements-batch
*            _scope         = '2'
*          EXCEPTIONS
*            foreign_lock   = 1
*            system_failure = 2
*            OTHERS         = 3.

        CALL FUNCTION 'ENQUEUE_EMMCH1E'
          EXPORTING
            mode_mch1      = 'E'
            mandt          = sy-mandt
            matnr          = lv_material
            charg          = wa_goodsmovements-batch
            _scope         = '2'
          EXCEPTIONS
            foreign_lock   = 1
            system_failure = 2
            OTHERS         = 3.
* <--- S4 Migration - 20/06/2023 - MA
        sperr_user     = sy-msgv1.

        IF sy-subrc <> 0.
          IF sperr_user IS NOT INITIAL.
            fg_bloqueio = 'X'.
            wa_return-type    = 'E'.
            wa_return-number  = 2.
            CONCATENATE 'Lote ' wa_goodsmovements-batch 'bloqueado por ' sperr_user INTO wa_return-message SEPARATED BY space.
            APPEND wa_return TO it_return.
          ENDIF.
        ENDIF.
      ENDIF.

    ENDLOOP.

    IF fg_bloqueio = 'X'.
      "desbloqueio
      LOOP AT it_goodsmovements INTO wa_goodsmovements.
* ---> S4 Migration - 20/06/2023 - MA
*        CALL FUNCTION 'DEQUEUE_EMMARCE'
*          EXPORTING
*            matnr = wa_goodsmovements-material
*            werks = wa_goodsmovements-plant.
*        "Lote
*        IF wa_goodsmovements-batch IS NOT INITIAL.
*          CALL FUNCTION 'DEQUEUE_EMMCH1E'
*            EXPORTING
*              mode_mch1 = 'E'
*              mandt     = sy-mandt
*              matnr     = wa_goodsmovements-material
*              charg     = wa_goodsmovements-batch.
*        ENDIF.

        lv_material = wa_goodsmovements-material.

        CALL FUNCTION 'DEQUEUE_EMMARCE'
          EXPORTING
            matnr = lv_material
            werks = wa_goodsmovements-plant.
        "Lote
        IF wa_goodsmovements-batch IS NOT INITIAL.
          CALL FUNCTION 'DEQUEUE_EMMCH1E'
            EXPORTING
              mode_mch1 = 'E'
              mandt     = sy-mandt
              matnr     = lv_material
              charg     = wa_goodsmovements-batch.
        ENDIF.
* <--- S4 Migration - 20/06/2023 - MA
      ENDLOOP.
    ENDIF.

    IF it_return[] IS NOT INITIAL.
      PERFORM: f_monta_msg_fd.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS'"#EC CI_USAGE_OK[2438131]
      EXPORTING
        bflushflags    = es_bflushflags
        bflushdatagen  = es_bflushdatagen
*       BFLUSHDATAMTS  =
      IMPORTING
        confirmation   = es_confirmation
        return         = wa_return
      TABLES
        goodsmovements = it_goodsmovements.

    IF wa_return-type = 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      APPEND wa_return TO it_return.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_x.

      SELECT SINGLE *
         INTO gs_blpp
         FROM blpp
         WHERE prtnr = es_confirmation
         AND prtps = '0001'.


      WAIT UP TO 2 SECONDS.


      SELECT SINGLE * FROM mseg
        INTO @DATA(wa_mseg)
       WHERE mblnr EQ @gs_blpp-belnr
        AND  mjahr EQ @wa_zppt0002_tot-budat+0(4)
        AND  bwart EQ '261'.

      IF sy-subrc NE 0.

        CALL FUNCTION 'BAPI_REPMANCONF1_CANCEL'
          EXPORTING
            confirmation     = es_confirmation
          IMPORTING
            cancconfirmation = v_confirmation_es
            return           = wa_return.

        IF wa_return-type = 'E'.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          APPEND wa_return TO it_return.
        ELSE.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = c_x.

          SELECT SINGLE *
             INTO @DATA(gs_blpp_es)
             FROM blpp
             WHERE prtnr = @v_confirmation_es
             AND prtps = '0001'.


          WAIT UP TO 2 SECONDS.
          DATA(doc_estorno) = gs_blpp_es-belnr.


          wa_return-type       = 'E'.
          CONCATENATE 'Documento de Fardão gerado/estornado:' gs_blpp-belnr ' / ' doc_estorno INTO wa_return-message.
          wa_return-number  = 2.
          APPEND wa_return TO it_return.

        ENDIF.
      ELSE.
        UPDATE zppt0002
            SET status_registro = '02' "Libera para TRACECOTTON classificar
                mblnr02         = gs_blpp-belnr
          WHERE charg  = wa_zppt0002_tot-charg
            AND werks  = wa_zppt0002_tot-werks
            AND status_registro = '01'.

        COMMIT WORK AND WAIT .

        "ZCL_WEBSERVICE_trace

        "desbloqueio

        LOOP AT it_goodsmovements INTO wa_goodsmovements.
* ---> S4 Migration - 20/06/2023 - MA
*          CALL FUNCTION 'DEQUEUE_EMMARCE'
*            EXPORTING
*              matnr = wa_goodsmovements-material
*              werks = wa_goodsmovements-plant.

          lv_material = wa_goodsmovements-material.

          CALL FUNCTION 'DEQUEUE_EMMARCE'
            EXPORTING
              matnr = lv_material
              werks = wa_goodsmovements-plant.
* <--- S4 Migration - 20/06/2023 - MA
        ENDLOOP.

        wa_return-type       = 'S'.
        CONCATENATE 'Documento de Fardão gerado:' gs_blpp-belnr INTO wa_return-message.
        wa_return-message_v1 = '02'.
        wa_return-message_v2 = gs_blpp-belnr.
        APPEND wa_return TO it_return.
      ENDIF.
    ENDIF.

    IF it_return[] IS NOT INITIAL.
      PERFORM: f_monta_msg_fd.
    ENDIF.

  ENDLOOP.

ENDFORM .                    "F_FARDAO

*&---------------------------------------------------------------------*
*&      Form  F_ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_fardinho.
  CHECK it_zppt0002 IS NOT INITIAL.

  SELECT *
  FROM zpps_ximfbf_log
  INTO TABLE @DATA(it_zpps_ximfbf_log)
  FOR ALL ENTRIES IN @it_zppt0002
  WHERE id_cotton = @it_zppt0002-id_cotton
  AND NOT EXISTS ( SELECT * FROM mseg WHERE mseg~smbln = zpps_ximfbf_log~mblnr  ).

  SORT it_zpps_ximfbf_log BY id_cotton.

  SELECT *
    FROM zmmt0006 AS m1
    INTO TABLE @DATA(it_zmmt0006)
    FOR ALL ENTRIES IN @it_zppt0002_far
    WHERE id_cotton = @it_zppt0002_far-id_cotton
    AND  ch_referencia = ( SELECT MAX( m2~ch_referencia ) FROM zmmt0006 AS m2 WHERE m2~id_cotton = m1~id_cotton ) .

  DELETE it_zmmt0006 WHERE tcode  = 'MBST'.

  SORT it_zmmt0006 BY id_cotton.

  LOOP AT it_zppt0002 INTO wa_zppt0002.
    DATA(tabix) = sy-tabix.
    IF wa_zppt0002-id_cotton IS NOT  INITIAL.
      READ TABLE it_zpps_ximfbf_log INTO DATA(wa_zpps_ximfbf_log) WITH KEY id_cotton = wa_zppt0002-id_cotton BINARY SEARCH.
      IF sy-subrc = 0.
        wa_zppt0002-charg = wa_zpps_ximfbf_log-charg.
        MODIFY it_zppt0002 FROM wa_zppt0002 INDEX tabix TRANSPORTING charg.
      ELSE.
        READ TABLE it_zmmt0006 INTO DATA(wa_zmmt0006)  WITH KEY id_cotton = wa_zppt0002-id_cotton BINARY SEARCH.
        IF sy-subrc = 0.
          wa_zppt0002-charg = wa_zmmt0006-batch_d.
          MODIFY it_zppt0002_far FROM wa_zppt0002 INDEX tabix TRANSPORTING charg.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  MODIFY zppt0002 FROM TABLE it_zppt0002.
  COMMIT WORK.


  SORT it_zppt0002 BY acharg matnr.

  DATA: it_mkal TYPE TABLE OF mkal,
        it_mara TYPE TABLE OF mara,
        it_stpo TYPE TABLE OF stpo.

  DATA: wa_mkal TYPE mkal,
        wa_stpo TYPE stpo.

** SELECIONAR MATERIAL
  SELECT * FROM mara
    INTO TABLE it_mara
    FOR ALL ENTRIES IN it_zppt0002
  WHERE matnr EQ it_zppt0002-matnr.

** SELECIONAR DEPOSITO
  SELECT * FROM stpo
    INTO TABLE it_stpo
    FOR ALL ENTRIES IN it_zppt0002
  WHERE idnrk EQ it_zppt0002-matnr.

  SORT: it_mkal BY matnr werks verid,
        it_mara BY matnr,
        it_stpo BY idnrk.

  CLEAR: wa_zppt0002, wa_saida.
  LOOP AT it_zppt0002 INTO wa_zppt0002.
    wa_saida-row               = sy-tabix.
    wa_saida-werks             = wa_zppt0002-werks.             "Fazenda de origem do fardinho
    wa_saida-verid             = wa_zppt0002-verid.             "Numero maquina
    wa_saida-acharg            = wa_zppt0002-acharg.            "Numero LOTE fardinho
    wa_saida-id_fardinho       = wa_zppt0002-id_fardinho.       "Numero fardinho
    wa_saida-matnr_con         = wa_zppt0002-matnr.             "Numero material
    wa_saida-menge_con         = wa_zppt0002-menge.             "Peso liquido
    wa_saida-menge             = wa_zppt0002-menge.             "Peso liquido
    wa_saida-charg_con         = wa_zppt0002-charg.             "Origem fardão
    wa_saida-id_fardao         = wa_zppt0002-id_fardao.         "Origem fardão
    wa_saida-budat_con         = wa_zppt0002-budat.             "Data de lançamento do fardão
    wa_saida-budat             = wa_zppt0002-dt_classificacao. "WA_ZPPT0002-BUDAT.             "Data de lançamento do fardão
    wa_saida-bldat             = wa_zppt0002-dt_classificacao. " WA_ZPPT0002-BLDAT.             "Data no documento
    wa_saida-cd_classificacao  = wa_zppt0002-cd_classificacao.  "Tipo do algodão
    wa_saida-status_con        = wa_zppt0002-status.            "Status
    wa_saida-mblnr             = wa_zppt0002-mblnr.             "N° Documento Material
    wa_saida-cd_safra          = wa_zppt0002-cd_safra.
    wa_saida-id_cotton         = wa_zppt0002-id_cotton.

    READ TABLE it_mara INTO wa_mara WITH KEY matnr = wa_zppt0002-matnr
                                    BINARY SEARCH.
    wa_saida-meins_con  = wa_mara-meins.

*    READ TABLE it_stpo INTO wa_stpo WITH KEY idnrk = wa_zppt0002-matnr
*                                    BINARY SEARCH.
*    wa_saida-lgort_con  = wa_stpo-lgort.
    CLEAR  wa_mkal.
    SELECT SINGLE * FROM mkal INTO wa_mkal WHERE werks = wa_saida-werks
                                           AND matnr = wa_saida-matnr_con
                                           AND verid = wa_saida-verid.
    IF wa_mkal-elpro IS NOT INITIAL.
      wa_saida-lgort_con  = wa_mkal-elpro.
    ELSE.
      wa_saida-lgort_con  = wa_mkal-alort.
    ENDIF.
    wa_saida-normt      = wa_zppt0002-cd_classificacao.
    wa_saida-alort      = wa_zppt0002-lgort.

    IF wa_saida-normt IS NOT INITIAL.
      SELECT SINGLE * FROM mara INTO wa_mara WHERE normt = wa_saida-normt AND mtart = 'ZFER'.
      IF sy-subrc = 0.
        MOVE: wa_mara-matnr TO wa_saida-matnr.
        MOVE: wa_mara-meins TO wa_saida-meins.
      ELSE.
        wa_saida-normt = ''.
      ENDIF.
    ELSE.
      wa_saida-matnr = ''.
    ENDIF.

    IF wa_saida-matnr IS NOT INITIAL.  "wa_saida-n_verid IS INITIAL.
      SELECT SINGLE * FROM mkal INTO wa_mkal WHERE werks = wa_saida-werks
                                               AND matnr = wa_saida-matnr
                                               AND verid = wa_saida-verid.

      IF sy-subrc = 0.
        MOVE: wa_mkal-text1 TO wa_saida-n_verid.
      ENDIF.
    ELSE.
      wa_saida-n_verid = ''.
    ENDIF.

    CLEAR: wa_zppt0006.
    FREE: it_zppt0006, it_return, wa_return.
    IF wa_saida-matnr IS INITIAL OR wa_saida-normt IS INITIAL.
      wa_return-type    = 'E'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_saida-matnr
        IMPORTING
          output = wa_saida-matnr.
      CONCATENATE 'Falta classificação ' wa_saida-matnr 'Dep.' wa_saida-alort 'Lote' wa_saida-acharg INTO wa_return-message SEPARATED BY space.
      APPEND wa_return TO it_return.

      PERFORM: f_monta_msg.
      CONTINUE.

    ENDIF.

    "checa se encontrou o lote de produção / transferencia
    FREE: it_zppt0006, it_return, wa_return.
    IF wa_zppt0002-charg IS INITIAL.
      wa_return-type    = 'E'.
      wa_return-number  = 1.
      CONCATENATE 'Não existe entrada de prod. p/ o rolo'   wa_zppt0002-id_cotton INTO wa_return-message SEPARATED BY space.
      APPEND wa_return TO it_return.
      PERFORM: f_monta_msg.
      CONTINUE.
    ENDIF.

    FREE: it_zppt0006, it_return, wa_return.
    IF wa_zppt0002-status_registro EQ '03'.
      READ TABLE it_mchb2 INTO wa_mchb2 WITH KEY charg = wa_saida-acharg
                                                 werks = wa_saida-werks.
      IF sy-subrc IS INITIAL.
        READ TABLE it_mseg1 INTO wa_mseg1 WITH KEY charg = wa_saida-acharg.
        IF sy-subrc IS INITIAL.

          UPDATE zppt0002
            SET status_registro = '04'
                mblnr           = wa_mseg1-mblnr
          WHERE acharg  = wa_saida-acharg
            AND werks   = wa_saida-werks
            AND status_registro = '03'.

          wa_return-type       = 'S'.
          wa_return-message_v1 = '04'.
          wa_return-message_v2 = wa_mseg1-mblnr.
          wa_return-message    = |Documento de Fardinho gerado:{ wa_mseg1-mblnr } R|.
          APPEND wa_return TO it_return.

          PERFORM: f_monta_msg.
          CONTINUE.

        ENDIF.
      ENDIF.
    ENDIF.

    APPEND wa_saida TO it_saida.
    CLEAR: wa_zppt0002, wa_saida.

  ENDLOOP.

ENDFORM.                    " F_ORGANIZA_DADOS


*&---------------------------------------------------------------------*
*&      Form  F_GRAVAR_DADOS
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_gravar_dados.
  CLEAR: wa_zppt0006. REFRESH: it_zppt0006.
  LOOP AT it_saida INTO wa_saida.
    CLEAR: es_bflushflags, es_bflushdatagen, wa_return.
    REFRESH: it_return, it_goodsmovements, it_bapi_char_batch.

    es_bflushflags-bckfltype        = '01'.

    es_bflushdatagen-postdate       = wa_saida-budat.       "'Data de Lançamento (BUDAT)'
    es_bflushdatagen-docdate        = wa_saida-bldat.       "Data do Documento (BLDAT)'
    es_bflushdatagen-prodplant      = wa_saida-werks.       "'Centro (WERKS)'

* > 05/07/2023 - Migração S4 - LM
*    es_bflushdatagen-materialnr     = wa_saida-matnr.       "'Material (MATNR)'

    IF STRLEN( wa_saida-matnr ) > 18.
      es_bflushdatagen-materialnr_long = wa_saida-matnr.   "'Material Consumo (MATNR) - LONG
    ELSE.
      es_bflushdatagen-materialnr      = wa_saida-matnr.   "'Material Consumo (MATNR)
    ENDIF.
* > 05/07/2023 - Migração S4 - LM

    es_bflushdatagen-backflquant    = wa_saida-menge.       "'Quantidade (ERFMG)'
    es_bflushdatagen-unitofmeasure  = wa_saida-meins.       "'UM (ERFME)'
    es_bflushdatagen-prodversion    = wa_saida-verid.       "'Versão (VERID)'
    es_bflushdatagen-batch          = wa_saida-acharg.      "'Lote (ACHARG)'
    es_bflushdatagen-storageloc     = wa_saida-alort.       "'Deposito (ALORT)'

* > 05/07/2023 - Migração S4 - LM
*    wa_goodsmovements-material      = wa_saida-matnr_con.   "'Material Consumo (MATNR)'

    IF STRLEN( wa_saida-matnr_con ) > 18.
      wa_goodsmovements-material_long = wa_saida-matnr_con.   "'Material Consumo (MATNR) - LONG
    ELSE.
      wa_goodsmovements-material      = wa_saida-matnr_con.   "'Material Consumo (MATNR)'
    ENDIF.
* > 05/07/2023 - Migração S4 - LM

    wa_goodsmovements-plant         = wa_saida-werks.       "'Centro (WERKS)'
    wa_goodsmovements-stge_loc      = wa_saida-lgort_con.   "'Deposito Consumo (LGORT)'
    wa_goodsmovements-batch         = wa_saida-charg_con.   "'Lote Consumo (CHARG)'
    wa_goodsmovements-move_type     = '261'.
    wa_goodsmovements-entry_qnt     = wa_saida-menge_con.   "'Quantidade consumo (ERFME_R)'
    wa_goodsmovements-entry_uom     = wa_saida-meins_con.   "'UM consumo (ERFME)'
    APPEND wa_goodsmovements TO it_goodsmovements.

*    WA_BAPI_CHAR_BATCH-CHAR_NAME   = 'Safra'.
*    WA_BAPI_CHAR_BATCH-CHAR_VALUE  = WA_SAIDA-CD_SAFRA.
*    APPEND WA_BAPI_CHAR_BATCH TO IT_BAPI_CHAR_BATCH.
*    CLEAR: VSALDO.


    SELECT SINGLE clabs
      FROM mchb
      INTO vsaldo
      WHERE matnr = wa_saida-matnr_con
      AND   werks = wa_saida-werks
      AND   lgort = wa_saida-lgort_con
      AND   charg = wa_saida-charg_con.

    IF wa_saida-menge_con GT vsaldo.
      wa_return-type    = 'E'.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
        EXPORTING
          input  = wa_saida-matnr_con
        IMPORTING
          output = wa_saida-matnr_con.
      CONCATENATE 'Falta estoque ' wa_saida-matnr_con 'Dep.' wa_saida-lgort_con 'Lote' wa_saida-charg_con INTO wa_return-message SEPARATED BY space.
      APPEND wa_return TO it_return.
    ENDIF.

    IF it_return[] IS NOT INITIAL.
      PERFORM: f_monta_msg.
      CONTINUE.
    ENDIF.

    CALL FUNCTION 'BAPI_REPMANCONF1_CREATE_MTS'"#EC CI_USAGE_OK[2438131]
      EXPORTING
        bflushflags    = es_bflushflags
        bflushdatagen  = es_bflushdatagen
*       BFLUSHDATAMTS  =
      IMPORTING
        confirmation   = es_confirmation
        return         = wa_return
      TABLES
*       SERIALNR       =
        goodsmovements = it_goodsmovements.
*        CHARACTERISTICS_BATCH = IT_BAPI_CHAR_BATCH.

    IF wa_return-type = 'E'.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      APPEND wa_return TO it_return.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_x.

      SELECT SINGLE *
          INTO gs_blpp
          FROM blpp
          WHERE prtnr = es_confirmation
          AND prtps = '0001'.

      UPDATE zppt0002
        SET status_registro = '04'
            mblnr           = gs_blpp-belnr
      WHERE acharg  = wa_saida-acharg
        AND werks   = wa_saida-werks
        AND status_registro = '03'.
      COMMIT WORK AND WAIT .

      wa_return-type       = 'S'.
      CONCATENATE 'Documento de Fardinho gerado:' gs_blpp-belnr INTO wa_return-message.
      wa_return-message_v1 = '04'.
      wa_return-message_v2 = gs_blpp-belnr.
      APPEND wa_return TO it_return.
    ENDIF.

    IF it_return[] IS NOT INITIAL.
      PERFORM: f_monta_msg.
      " troca Safra pela safra do trace
      vobjecttable = 'MCH1'.
      vclassnum = 'FARDOES'.
      vclasstype = '023'.
      CONCATENATE  wa_saida-matnr  wa_saida-acharg INTO v_objek.
      REFRESH it_allocvalues.
      wa_allocvalues-charact        = 'SAFRA'.
      wa_allocvalues-value_char     = wa_saida-cd_safra.
      wa_allocvalues-value_neutral  = wa_saida-cd_safra.
      wa_allocvalues-charact_descr  = 'Safra'.
      APPEND wa_allocvalues TO it_allocvalues.
* ---> S4 Migration - 20/06/2023 - MA
*      CALL FUNCTION 'BAPI_OBJCL_CHANGE'
*        EXPORTING
*          objectkey          = v_objek
*          objecttable        = vobjecttable
*          classnum           = vclassnum
*          classtype          = vclasstype
*        TABLES
*          allocvaluesnumnew  = it_allocnum
*          allocvaluescharnew = it_allocvalues
*          allocvaluescurrnew = it_alloccur
*          return             = it_return2.

      CALL FUNCTION 'BAPI_OBJCL_CHANGE'"#EC CI_USAGE_OK[2438131]
        EXPORTING
          objecttable        = vobjecttable
          classnum           = vclassnum
          classtype          = vclasstype
          objectkey_long     = v_objek
        TABLES
          allocvaluesnumnew  = it_allocnum
          allocvaluescharnew = it_allocvalues
          allocvaluescurrnew = it_alloccur
          return             = it_return2.
* <--- S4 Migration - 20/06/2023 - MA      .


      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_x.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " F_GRAVAR_DADOS


*&---------------------------------------------------------------------*
*&      Form  F_MONTA_MSG
*&---------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
FORM f_monta_msg_fd.
** MONTAR MSG DE SUCESSO E ERRO
  LOOP AT it_return INTO wa_return.

    IF wa_return-type = 'E'.
      wa_return-message_v1 = 'E2'. "Erro do fardão
    ENDIF.
    CLEAR wa_zppt0006.

    SELECT MAX( id ) INTO vid
       FROM zppt0006
       WHERE charg  = wa_zppt0002_tot-charg
       AND   acharg = wa_zppt0002_tot-charg
       AND   werks  = wa_zppt0002_tot-werks.
*       AND   id_cotton  = WA_ZPPT0002_TOT-id_cotton..

    "
    ADD 1 TO vid.
    wa_zppt0006-charg        = wa_zppt0002_tot-charg.
    wa_zppt0006-acharg       = wa_zppt0002_tot-charg. "somente fardão
    wa_zppt0006-id_fardao    = wa_zppt0002_tot-id_fardao.
    wa_zppt0006-id_fardinho  = 0. "somente fardão
    wa_zppt0006-id           = vid.
    wa_zppt0006-budat        = wa_zppt0002_tot-budat.
    wa_zppt0006-werks        = wa_zppt0002_tot-werks.
    wa_zppt0006-matnr        = wa_zppt0002_tot-matnr.
    wa_zppt0006-verid        = wa_zppt0002_tot-verid.
    wa_zppt0006-msgnr        = wa_return-number.
    wa_zppt0006-status_msg   = wa_return-type.
    wa_zppt0006-cd_mensagem  = wa_return-message.
    wa_zppt0006-data         = sy-datum.
    wa_zppt0006-hora         = sy-uzeit.
    wa_zppt0006-id_cotton    = wa_zppt0002_tot-id_cotton.
    wa_zppt0006-flag_envio   = 'R'.
    wa_zppt0006-status_registro   = wa_return-message_v1+0(3).
    APPEND wa_zppt0006 TO it_zppt0006.
    IF wa_zppt0002-id_cotton IS INITIAL.
      CONCATENATE wa_zppt0002_tot-charg '|' wa_zppt0002_tot-werks INTO vg_obj_key.
    ELSE.
      CONCATENATE wa_zppt0002-id_cotton '|' wa_zppt0002_tot-werks INTO vg_obj_key.
    ENDIF.

    PERFORM z_prepara_mensagem   USING vg_obj_key
                                       wa_return-type
                                       wa_return-message
                                       wa_return-message_v1
                                       wa_return-message_v2
                                       vg_interface.
  ENDLOOP.
  MODIFY zppt0006 FROM TABLE it_zppt0006.
  UPDATE zppt0002 SET status = 'P'
                      status_registro = wa_return-message_v1+0(3)
    WHERE charg            EQ wa_zppt0002_tot-charg
    AND   id_cotton        EQ wa_zppt0002_tot-id_cotton "30.07.2020 ALRS
    AND   status           EQ 'R'.
  COMMIT WORK AND WAIT .
ENDFORM.                    " F_MONTA_MSG

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_monta_msg.

** MONTAR MSG DE SUCESSO E ERRO fardinho
  LOOP AT it_return INTO wa_return.
*    IF WA_RETURN-TYPE = 'E'. "controle para gravar apenas uma vez se for erro
*      SELECT SINGLE *
*        INTO WA_ZPPT0006
*        FROM ZPPT0006
*         WHERE CHARG  = WA_SAIDA-CHARG_CON
*         AND   ACHARG = WA_SAIDA-ACHARG
*         AND   WERKS  = WA_SAIDA-WERKS
*         AND   MSGNR  = WA_RETURN-NUMBER.
*
*      IF SY-SUBRC = 0.
*        CONTINUE. "Não grava mesmo erro
*      ENDIF.
*    ENDIF.
    CLEAR wa_zppt0006.
    IF wa_return-type = 'E'.
      wa_return-message_v1 = 'E4'. "Erro do fardinho
    ENDIF.

    SELECT MAX( id ) INTO vid
       FROM zppt0006
       WHERE charg  = wa_saida-charg_con
       AND   acharg = wa_saida-acharg
       AND   werks  = wa_saida-werks.
    "
    ADD 1 TO vid.
    wa_zppt0006-charg             = wa_saida-charg_con.
    wa_zppt0006-acharg            = wa_saida-acharg.
    wa_zppt0006-id_fardinho       = wa_saida-id_fardinho.
    wa_zppt0006-id_fardao         = wa_saida-id_fardao.
    wa_zppt0006-id                = vid.
    wa_zppt0006-budat             = wa_saida-budat.
    wa_zppt0006-werks             = wa_saida-werks.
    wa_zppt0006-matnr             = wa_saida-matnr.
    wa_zppt0006-verid             = wa_saida-verid.
    wa_zppt0006-lgort             = wa_saida-alort.
    wa_zppt0006-cd_classificacao  = wa_saida-cd_classificacao.
    wa_zppt0006-msgnr             = wa_return-number.
    wa_zppt0006-status_msg        = wa_return-type.
    wa_zppt0006-cd_mensagem       = wa_return-message.
    wa_zppt0006-data              = sy-datum.
    wa_zppt0006-hora              = sy-uzeit.
    wa_zppt0006-id_cotton         = wa_saida-id_cotton.
    wa_zppt0006-flag_envio        = 'R'.
    wa_zppt0006-status_registro   = wa_return-message_v1+0(3).
    APPEND wa_zppt0006 TO it_zppt0006.

    CONCATENATE wa_saida-acharg '|' wa_saida-werks INTO vg_obj_key.

    PERFORM z_prepara_mensagem   USING vg_obj_key
                                       wa_return-type
                                       wa_return-message
                                       wa_return-message_v1
                                       wa_return-message_v2
                                       vg_interface.
  ENDLOOP.
  MODIFY zppt0006 FROM TABLE it_zppt0006.
  UPDATE zppt0002 SET status = 'P'
                      status_registro = wa_return-message_v1+0(3)
   WHERE charg             = wa_saida-charg_con
   AND   acharg            = wa_saida-acharg
   AND   status           EQ 'R'.
  COMMIT WORK AND WAIT .
ENDFORM.                    " F_MONTA_MSG

*&---------------------------------------------------------------------*
*&      Form  Z_PREPARA_MENSAGEM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_OBJ_KEY    text
*      -->P_TYPE       text
*      -->P_MESSAGE    text
*      -->P_INTERFACE  text
*----------------------------------------------------------------------*
FORM z_prepara_mensagem  USING    p_obj_key    TYPE any
                                  p_type       TYPE any
                                  p_message    TYPE any
                                  p_message_v1 TYPE any
                                  p_message_v2 TYPE any
                                  p_interface  TYPE any.

  wa_outreturn-obj_key        = p_obj_key.
  wa_outreturn-interface      = p_interface.
  wa_outreturn-id             = p_interface.
  wa_outreturn-num            = p_interface.
  wa_outreturn-message        = p_message.
  wa_outreturn-message_v1     = p_message_v1.
  wa_outreturn-message_v2     = p_message_v2.
  wa_outreturn-type           = p_type.
  wa_outreturn-dt_atualizacao = sy-datum.
  wa_outreturn-hr_atualizacao = sy-uzeit.

  APPEND wa_outreturn TO it_outreturn.
  CLEAR wa_outreturn.

ENDFORM.                    "Z_PREPARA_MENSAGEM
*&---------------------------------------------------------------------*
*&      Form  F_TRANSFERENCIA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_transferencia .
  CHECK it_zppt0002_tra IS NOT INITIAL.
  SORT it_zppt0002_tra BY werks acharg.
  DATA: tl_return        TYPE TABLE OF bapiret2,
        sl_return        TYPE bapiret2,
        tabix            TYPE sy-tabix,
        st_qtd_bloco(5),
        st_qtd_fardao(5),
        vg_qtd_bloco     TYPE zppt0002-qtd_bloco.



  REFRESH: tl_return, t_trans.
  CLEAR sl_return.
  "
  it_zppt0002_tot[] = it_zppt0002_tra[].

  DELETE ADJACENT DUPLICATES FROM it_zppt0002_tot COMPARING werks lgort.

  LOOP AT it_zppt0002_tot INTO wa_zppt0002_tot.
    vg_qtd_bloco      = 0.
    tabix = sy-tabix.
    LOOP AT it_zppt0002_tra INTO wa_zppt0002 WHERE werks = wa_zppt0002_tot-werks
                                             AND   lgort = wa_zppt0002_tot-lgort.

      ADD 1 TO vg_qtd_bloco.
    ENDLOOP.
    "checa a quantidade de blocos do lote
    IF vg_qtd_bloco NE wa_zppt0002_tot-qtd_bloco AND wa_zppt0002_tot-qtd_bloco GT 0.
      wa_zppt0002_tot-status_registro = '999'.
      MODIFY it_zppt0002_tot FROM wa_zppt0002_tot INDEX tabix TRANSPORTING status_registro.
    ENDIF.
  ENDLOOP.
  "
  "Pega informações do lote do fardinho no estoque
  LOOP AT it_zppt0002_tra INTO wa_zppt0002.
    READ TABLE it_zppt0002_tot INTO wa_zppt0002_tot WITH KEY werks = wa_zppt0002-werks
                                                             lgort = wa_zppt0002-lgort BINARY SEARCH.
    IF sy-subrc = 0.
      IF wa_zppt0002_tot-status_registro = '999'.
        "
        sl_return-type = 'E'.
        IF wa_zppt0002-status_registro = '07'.
          sl_return-message_v1 = 'E8'. "Erro  transf. deposito
          CONCATENATE 'Bloco' wa_zppt0002-lgort 'transf. deposito faltando fardinhos' INTO sl_return-message SEPARATED BY space.
        ELSE.
          sl_return-message_v1 = 'E10'. "Erro  transf. material
          CONCATENATE 'Bloco' wa_zppt0002-lgort 'transf. material faltando fardinhos' INTO sl_return-message SEPARATED BY space.
        ENDIF.
        APPEND sl_return TO tl_return.
        "
        it_return[] = tl_return[].
        sl_trans-werks  = wa_zppt0002-werks.
        sl_trans-charg = wa_zppt0002-acharg.
        sl_trans-status_registro = wa_zppt0002-status_registro.
        PERFORM f_monta_msg_tra.
        CONTINUE.
      ENDIF.
    ENDIF.
    SELECT SINGLE matnr werks lgort
        charg clabs cspem
         FROM mchb
         INTO  sl_trans
           WHERE  charg = wa_zppt0002-acharg "Fardinho
             AND  werks = wa_zppt0002-werks
             AND  clabs GT 0.

    IF sy-subrc = 0.
      sl_trans-status_registro = wa_zppt0002-status_registro.
      IF wa_zppt0002-status_registro = '07'.
        sl_trans-lgortr = wa_zppt0002-lgort. "Deposito receptor é o que foi alterado no TRACE
      ELSE. "troca de material
        SELECT SINGLE * FROM mara INTO wa_mara WHERE normt = wa_zppt0002-cd_classificacao AND mtart = 'ZFER'. "classificacao nova
        IF sy-subrc = 0.
          sl_trans-matnc  = wa_mara-matnr.
          IF sl_trans-lgort NE wa_zppt0002-lgort.
            sl_trans-lgortr = wa_zppt0002-lgort. "Deposito receptor é o que foi alterado no TRACE
          ENDIF.
        ENDIF.
      ENDIF.
      APPEND sl_trans TO t_trans.
      CLEAR sl_trans.
    ELSE.
      sl_trans-werks  = wa_zppt0002-werks.
      sl_trans-charg = wa_zppt0002-acharg.
      sl_trans-status_registro = wa_zppt0002-status_registro.
      sl_return-type = 'E'.
      IF wa_zppt0002-status_registro = '07'.
        sl_return-message_v1 = 'E8'. "Erro  transf. deposito
        CONCATENATE 'Sem Saldo Livre, fardinho' wa_zppt0002-acharg 'transf. deposito' INTO sl_return-message SEPARATED BY space.
      ELSE.
        sl_return-message_v1 = 'E10'. "Erro  transf. material
        CONCATENATE 'Sem Saldo Livre, fardinho' wa_zppt0002-acharg 'transf. material' INTO sl_return-message SEPARATED BY space.
      ENDIF.
      APPEND sl_return TO tl_return.

      it_return[] = tl_return[].
      PERFORM f_monta_msg_tra.
    ENDIF.
  ENDLOOP.

  PERFORM z_cria_trans.

ENDFORM.                    " F_TRANSFERENCIA

*&---------------------------------------------------------------------*
*&      Form  Z_CRIA_TRANS                                             *
*&---------------------------------------------------------------------*
*                           Cria Transferência                         *
*----------------------------------------------------------------------*
FORM z_cria_trans.

  DATA:
    sl_header   TYPE bapi2017_gm_head_01,
    vl_code     TYPE bapi2017_gm_code,
    vl_material TYPE bapi2017_gm_head_ret-mat_doc,
    vl_year     TYPE bapi2017_gm_head_ret-doc_year,
    tl_item     TYPE TABLE OF bapi2017_gm_item_create,
    tl_return   TYPE TABLE OF bapiret2,
    sl_return   TYPE bapiret2,
    sl_item     TYPE bapi2017_gm_item_create,
    sl_zmmt0008 TYPE zmmt0008,
    vl_index    TYPE i.


  CLEAR sl_header.


  LOOP AT t_trans INTO sl_trans.
    vl_index = sy-tabix.
    IF sl_trans-clabs IS INITIAL.
      sl_trans-clabs = sl_trans-cspem.
      MODIFY t_trans FROM sl_trans INDEX vl_index
        TRANSPORTING clabs.
    ENDIF.
    CLEAR sl_trans.
  ENDLOOP.

  DELETE t_trans WHERE clabs EQ 0.

  SORT t_trans BY matnr ASCENDING
                  werks ASCENDING
                  lgort ASCENDING
                  charg ASCENDING.

  LOOP AT t_trans INTO sl_trans.

    CLEAR: vl_material,
           vl_year    ,
           sl_zmmt0008.

    REFRESH: tl_item  ,
             tl_return.

    vl_code = '06'.
    sl_header-pstng_date = sy-datum.
    sl_header-doc_date   = sy-datum.

    IF sl_trans-status_registro = '07'.
      sl_item-move_type  = '311'.
    ELSE.
      sl_item-move_type  = '309'.
    ENDIF.

* > 05/07/2023 - Migração S4 - LM
*     sl_item-material   = sl_trans-matnr.

    IF STRLEN( sl_trans-matnr ) > 18.
      sl_item-material_long = sl_trans-matnr.   " LONG
    ELSE.
      sl_item-material      = sl_trans-matnr.
    ENDIF.
* > 05/07/2023 - Migração S4 - LM

    sl_item-plant      = sl_trans-werks.
    sl_item-stge_loc   = sl_trans-lgort.
    sl_item-batch      = sl_trans-charg.
    sl_item-entry_qnt  = sl_trans-clabs.
    sl_item-move_plant = sl_trans-werks.
    sl_item-move_stloc = sl_trans-lgortr.
    sl_item-move_mat   = sl_trans-matnc.
    "SL_ITEM-CUSTOMER   = P_KUNNR.
    "SL_HEADER-HEADER_TXT  = P_SGTXT.
    sl_item-move_batch = sl_trans-charg.


    APPEND sl_item TO tl_item.

    CALL FUNCTION 'BAPI_GOODSMVT_CREATE'"#EC CI_USAGE_OK[2438131]
      EXPORTING
        goodsmvt_header  = sl_header
        goodsmvt_code    = vl_code
      IMPORTING
        materialdocument = vl_material
        matdocumentyear  = vl_year
      TABLES
        goodsmvt_item    = tl_item
        return           = tl_return.

    IF NOT vl_material IS INITIAL.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = c_x.
      REFRESH tl_return.
      sl_return-type     = 'S'.
      CONCATENATE
                  vl_material
                  '/'
                  vl_year
             INTO sl_return-message SEPARATED BY space.

      IF sl_trans-status_registro = '07'.
        sl_return-message_v1 = '08'.
        CONCATENATE sl_return-message 'Transf. de bloco Gerado'  INTO sl_return-message SEPARATED BY space.
      ELSE.
        sl_return-message_v1 = '10'.
        IF sl_trans-lgortr NE sl_trans-lgortr AND sl_trans-lgortr IS NOT INITIAL.
          CONCATENATE sl_return-message 'Transf. de material/bloco Gerado'  INTO sl_return-message SEPARATED BY space.
        ELSE.
          CONCATENATE sl_return-message 'Transf. de material Gerado'  INTO sl_return-message SEPARATED BY space.
        ENDIF.
      ENDIF.
      APPEND sl_return TO tl_return.

      it_return[] = tl_return[].
      PERFORM f_monta_msg_tra.

      IF sl_trans-status_registro = '07'.
        UPDATE zppt0002 SET  status_registro = '08'
           WHERE acharg  = sl_trans-charg
             AND werks   = sl_trans-werks
             AND status_registro = '07'.
      ELSE.
        UPDATE zppt0002 SET  status_registro = '10'
         WHERE acharg  = sl_trans-charg
           AND werks   = sl_trans-werks
           AND status_registro = '09'.
      ENDIF.

      COMMIT WORK AND WAIT .

    ELSE.
*     Retorna Erro
      it_return[] = tl_return[].
      PERFORM f_monta_msg_tra.

      CLEAR: sl_trans,
             sl_item .
    ENDIF.
  ENDLOOP.


ENDFORM.                    " Z_CRIA_TRANS

*&---------------------------------------------------------------------*
*&      Form  F_MONTA_MSG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_monta_msg_tra.

** MONTAR MSG DE SUCESSO E ERRO fardinho
  LOOP AT it_return INTO wa_return.
    "LEITURA NOVAS INFORMAÇÕES DO FARDINHO
    READ TABLE it_zppt0002_tra INTO wa_zppt0002 WITH KEY werks  = sl_trans-werks
                                                         acharg = sl_trans-charg BINARY SEARCH.
    CLEAR wa_zppt0006.

    IF wa_return-type = 'E'.
      IF sl_trans-status_registro = '07'.
        wa_return-message_v1 = 'E8'. "Erro  transf. deposito
      ELSE.
        wa_return-message_v1 = 'E10'. "Erro transf. material
      ENDIF.
    ENDIF.

    SELECT MAX( id ) INTO vid
       FROM zppt0006
    WHERE charg  = wa_zppt0002-charg
    AND   acharg = wa_zppt0002-acharg
    AND   werks  = wa_zppt0002-werks.
    "
    ADD 1 TO vid.
    wa_zppt0006-charg             = wa_zppt0002-charg.
    wa_zppt0006-acharg            = wa_zppt0002-acharg.
    wa_zppt0006-id_fardao         = wa_zppt0002-id_fardao.

    wa_zppt0006-id_fardinho       = wa_zppt0002-id_fardinho.
    wa_zppt0006-id_fardao         = wa_zppt0002-id_fardao.
    wa_zppt0006-id                = vid.
    wa_zppt0006-budat             = wa_zppt0002-budat.
    wa_zppt0006-werks             = wa_zppt0002-werks.
    wa_zppt0006-matnr             = wa_zppt0002-matnr.
    wa_zppt0006-verid             = wa_zppt0002-verid.
    wa_zppt0006-lgort             = wa_zppt0002-lgort.            "NOVO
    wa_zppt0006-cd_classificacao  = wa_zppt0002-cd_classificacao. "NOVO
    wa_zppt0006-msgnr             = wa_return-number.
    wa_zppt0006-status_msg        = wa_return-type.
    wa_zppt0006-cd_mensagem       = wa_return-message.
    wa_zppt0006-data              = sy-datum.
    wa_zppt0006-hora              = sy-uzeit.
    wa_zppt0006-id_cotton         = wa_zppt0002-id_cotton.
    wa_zppt0006-flag_envio        = 'R'.
    wa_zppt0006-status_registro   = wa_return-message_v1+0(3).
    APPEND wa_zppt0006 TO it_zppt0006.

    CONCATENATE wa_zppt0002-acharg '|' wa_zppt0002-werks INTO vg_obj_key.

    PERFORM z_prepara_mensagem   USING vg_obj_key
                                       wa_return-type
                                       wa_return-message
                                       wa_return-message_v1
                                       wa_return-message_v2
                                       vg_interface.
  ENDLOOP.
  MODIFY zppt0006 FROM TABLE it_zppt0006.

  UPDATE zppt0002 SET status = 'P'
                      status_registro = wa_return-message_v1+0(3)
   WHERE charg             = wa_zppt0002-charg
   AND   acharg            = wa_zppt0002-acharg
   AND   status           EQ 'R'.

  COMMIT WORK AND WAIT .
ENDFORM.                    " F_MONTA_MSG

FORM f_envia_trace.
  DATA: e_fardo     TYPE zpme0059.
  DATA: t_fardo     TYPE zpmt0059.

  DATA: it_zppt0006_tot TYPE TABLE OF zppt0006.
  DATA: it_zppt0006_tot_c TYPE TABLE OF zppt0006.

  DATA: obj_trace TYPE REF TO zcl_webservice_trace.
  FREE: obj_trace.

  CREATE OBJECT: obj_trace.

  SELECT *
    FROM zppt0006 AS a
    INTO TABLE @DATA(t_zppt0006)
    WHERE a~flag_envio EQ 'R'
     AND a~id EQ ( SELECT MAX( id )
                    FROM zppt0006
                   WHERE flag_envio EQ 'R'
                     AND charg EQ a~charg
                     AND acharg EQ a~acharg
                 ).

  CHECK t_zppt0006[] IS NOT INITIAL.

  SORT t_zppt0006      BY werks charg acharg.

  it_zppt0006_tot[] = t_zppt0006[].

  it_zppt0006_tot_c[] = t_zppt0006[].

  DELETE ADJACENT DUPLICATES FROM it_zppt0006_tot COMPARING werks charg.

  DELETE ADJACENT DUPLICATES FROM it_zppt0006_tot_c COMPARING werks charg acharg.


  SELECT *
    FROM zppt0002
    INTO TABLE it_zppt0002_far
    FOR ALL ENTRIES IN it_zppt0006_tot
    WHERE werks = it_zppt0006_tot-werks
    AND   charg = it_zppt0006_tot-charg.

  SORT it_zppt0002_far BY werks charg acharg.

  DELETE it_zppt0006_tot WHERE status_registro NE '02' AND status_registro NE 'E2' AND status_registro NE '06'. "somente fardão
  LOOP AT it_zppt0006_tot INTO DATA(wa_zppt0006_tot).
    LOOP AT it_zppt0002_far INTO DATA(w_zppt0002) WHERE werks = wa_zppt0006_tot-werks
                                                  AND   charg = wa_zppt0006_tot-charg.
      e_fardo-nr_fardo              = w_zppt0002-acharg.
      e_fardo-id_filial_sap         = w_zppt0002-werks.
      e_fardo-nr_maquina            = w_zppt0002-verid.
*---> 14/06/2023 - Migração S4 - JS
*     e_fardo-id_material_sap       = w_zppt0002-matnr.
      e_fardo-id_material_sap = CONV #( w_zppt0002-matnr ).
*<--- 14/06/2023 - Migração S4 - JS
      e_fardo-quantidade            = w_zppt0002-menge.
      IF w_zppt0002-id_cotton IS INITIAL.
        e_fardo-nr_fardo_origem       = w_zppt0002-charg.
      ELSE.
        e_fardo-nr_fardo_origem       = w_zppt0002-id_cotton.
      ENDIF.
      e_fardo-dt_lancamento         = w_zppt0002-budat.
      CONCATENATE w_zppt0002-budat+0(4) '-' w_zppt0002-budat+4(2) '-' w_zppt0002-budat+6(2) INTO e_fardo-dt_lancamento.
      e_fardo-dt_documento          = w_zppt0002-bldat.
      CONCATENATE w_zppt0002-bldat+0(4) '-' w_zppt0002-bldat+4(2) '-' w_zppt0002-bldat+6(2) INTO e_fardo-dt_documento.
      e_fardo-dt_fabricacao         = w_zppt0002-dt_fabricacao.
      CONCATENATE w_zppt0002-dt_fabricacao+0(4) '-' w_zppt0002-dt_fabricacao+4(2) '-' w_zppt0002-dt_fabricacao+6(2) INTO e_fardo-dt_fabricacao.
      e_fardo-hr_fabricacao         = w_zppt0002-hr_fabricacao.
      e_fardo-status                = w_zppt0002-status.
      e_fardo-usuario               = w_zppt0002-usnam.
      e_fardo-safra                 = w_zppt0002-cd_safra.
      e_fardo-id_fardinho           = w_zppt0002-id_fardinho.
      e_fardo-cd_sai                = w_zppt0002-cd_sai.
      e_fardo-peso_bruto            = w_zppt0002-peso_bruto.
      e_fardo-peso_liquido          = w_zppt0002-peso_liquido.
      e_fardo-id_fardao             = w_zppt0002-id_fardao.
      e_fardo-cd_classificacao      = w_zppt0002-cd_classificacao.
      e_fardo-deposito              = w_zppt0002-lgort.
      e_fardo-doc_material_fardinho = w_zppt0002-mblnr.
      e_fardo-doc_material_fardao   = w_zppt0002-mblnr02.
      e_fardo-nome_responsavel      = ' '.
      e_fardo-dt_atualizacao        = sy-datum.
      CONCATENATE sy-datum+0(4) '-' sy-datum+4(2) '-' sy-datum+6(2) INTO e_fardo-dt_atualizacao.
      e_fardo-status_registro       = wa_zppt0006_tot-status_registro.
      e_fardo-cd_mensagem           = wa_zppt0006_tot-cd_mensagem.
      e_fardo-rg_atualizado         = 1.
      e_fardo-qtd_fardinhos         = w_zppt0002-qtd_fardinhos.
      e_fardo-peso_caroco           = w_zppt0002-peso_caroco.
      e_fardo-peso_fibrilha         = w_zppt0002-peso_fibrilha.
      e_fardo-rowversion            = 0.
      e_fardo-seq_num                = 0.
      APPEND e_fardo TO t_fardo.
      CLEAR e_fardo.
    ENDLOOP.


  ENDLOOP.

  DELETE it_zppt0006_tot_c WHERE status_registro EQ '02' OR status_registro EQ 'E2' OR status_registro EQ '06'. "somente fardinhos
  LOOP AT it_zppt0006_tot_c INTO wa_zppt0006_tot.

    READ TABLE it_zppt0002_far INTO w_zppt0002 WITH KEY  werks = wa_zppt0006_tot-werks
                                                         charg = wa_zppt0006_tot-charg
                                                         acharg = wa_zppt0006_tot-acharg.
    IF sy-subrc IS INITIAL.
      e_fardo-nr_fardo              = w_zppt0002-acharg.
      e_fardo-id_filial_sap         = w_zppt0002-werks.
      e_fardo-nr_maquina            = w_zppt0002-verid.
*---> 14/06/2023 - Migração S4 - JS
*       e_fardo-id_material_sap       = w_zppt0002-matnr.
      e_fardo-id_material_sap = CONV #( w_zppt0002-matnr ).
*<--- 14/06/2023 - Migração S4 - JS
      e_fardo-quantidade            = w_zppt0002-menge.
      IF w_zppt0002-id_cotton IS INITIAL.
        e_fardo-nr_fardo_origem       = w_zppt0002-charg.
      ELSE.
        e_fardo-nr_fardo_origem       = w_zppt0002-id_cotton.
      ENDIF.
      e_fardo-dt_lancamento         = w_zppt0002-budat.
      CONCATENATE w_zppt0002-budat+0(4) '-' w_zppt0002-budat+4(2) '-' w_zppt0002-budat+6(2) INTO e_fardo-dt_lancamento.
      e_fardo-dt_documento          = w_zppt0002-bldat.
      CONCATENATE w_zppt0002-bldat+0(4) '-' w_zppt0002-bldat+4(2) '-' w_zppt0002-bldat+6(2) INTO e_fardo-dt_documento.
      e_fardo-dt_fabricacao         = w_zppt0002-dt_fabricacao.
      CONCATENATE w_zppt0002-dt_fabricacao+0(4) '-' w_zppt0002-dt_fabricacao+4(2) '-' w_zppt0002-dt_fabricacao+6(2) INTO e_fardo-dt_fabricacao.
      e_fardo-hr_fabricacao         = w_zppt0002-hr_fabricacao.
      e_fardo-status                = w_zppt0002-status.
      e_fardo-usuario               = w_zppt0002-usnam.
      e_fardo-safra                 = w_zppt0002-cd_safra.
      e_fardo-id_fardinho           = w_zppt0002-id_fardinho.
      e_fardo-cd_sai                = w_zppt0002-cd_sai.
      e_fardo-peso_bruto            = w_zppt0002-peso_bruto.
      e_fardo-peso_liquido          = w_zppt0002-peso_liquido.
      e_fardo-id_fardao             = w_zppt0002-id_fardao.
      e_fardo-cd_classificacao      = w_zppt0002-cd_classificacao.
      e_fardo-deposito              = w_zppt0002-lgort.
      e_fardo-doc_material_fardinho = w_zppt0002-mblnr.
      e_fardo-doc_material_fardao   = w_zppt0002-mblnr02.
      e_fardo-nome_responsavel      = ' '.
      e_fardo-dt_atualizacao        = sy-datum.
      CONCATENATE sy-datum+0(4) '-' sy-datum+4(2) '-' sy-datum+6(2) INTO e_fardo-dt_atualizacao.
      e_fardo-status_registro       = wa_zppt0006_tot-status_registro.
      e_fardo-cd_mensagem           = wa_zppt0006_tot-cd_mensagem.
      e_fardo-rg_atualizado         = 1.
      e_fardo-qtd_fardinhos         = w_zppt0002-qtd_fardinhos.
      e_fardo-peso_caroco           = w_zppt0002-peso_caroco.
      e_fardo-peso_fibrilha         = w_zppt0002-peso_fibrilha.
      e_fardo-rowversion            = 0.
      e_fardo-seq_num                = 0.
      APPEND e_fardo TO t_fardo.
      CLEAR e_fardo.
    ENDIF.

  ENDLOOP.

  IF t_fardo[] IS NOT INITIAL.

    CALL METHOD obj_trace->atualiza_trace
      EXPORTING
        t_fardo  = t_fardo               " Estrutura para retorno
*      IMPORTING
*       id_referencia = DATA(id_referencia)   " Id Referencia do LOG da Integração
      RECEIVING
        ret_code = DATA(_ret_code).      " Status Code


    IF _ret_code EQ 200.

      LOOP AT it_zppt0006_tot INTO wa_zppt0006_tot.
        UPDATE zppt0006 SET flag_envio = 'P'
*                            id_referencia = id_referencia
           WHERE werks  = wa_zppt0006_tot-werks
           AND   charg  = wa_zppt0006_tot-charg
           AND   acharg = wa_zppt0006_tot-acharg
           AND   id_cotton = wa_zppt0006_tot-id_cotton
*              AND   ID     = WA_ZPPT0006_TOT-ID
           AND   flag_envio = 'R'.
      ENDLOOP.
      COMMIT WORK.

      LOOP AT it_zppt0006_tot_c INTO wa_zppt0006_tot.
        UPDATE zppt0006 SET flag_envio = 'P'
*                            id_referencia = id_referencia
           WHERE werks  = wa_zppt0006_tot-werks
           AND   charg  = wa_zppt0006_tot-charg
           AND   acharg = wa_zppt0006_tot-acharg
           AND   id_cotton = wa_zppt0006_tot-id_cotton
*              AND   ID     = WA_ZPPT0006_TOT-ID
           AND   flag_envio = 'R'.
      ENDLOOP.
      COMMIT WORK.

    ENDIF.
  ENDIF.

ENDFORM.
