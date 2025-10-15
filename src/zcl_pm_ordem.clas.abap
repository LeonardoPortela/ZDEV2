class ZCL_PM_ORDEM definition
  public
  final
  create public .

public section.

  methods CRIAR_ORDENS
    importing
      !I_ORDEM type ZPMT0016
      !I_OPERACAO type ZPMT0017_T
      !I_APONTAMENTO type ZPMT0015_T optional
      !I_ONLINE type CHAR1 optional
      !I_NOTAS type ZPMT0020_T optional
    exporting
      !E_ORDEM type AUFNR
      !E_MSG type BAPI_MSG
      !E_APONTAMENTOS type ZTPM_ITENS_APONT2 .
  methods GET_BLOCO
    returning
      value(E_BLOCO) type ZPMED005 .
  methods SET_IDORD .
  methods GET_IDORD
    returning
      value(E_IDOPR) type ZPMED002 .
  methods SET_IDOPR .
  methods GET_IDOPR
    returning
      value(E_IDOPR) type ZPMED003 .
  methods CONVERT_DATE
    importing
      !I_DATA type CHAR10
      !I_VERSAO type CHAR1 optional
    returning
      value(E_DATA) type SY-DATUM .
  methods CONVERT_TIME
    importing
      !I_TIME type CHAR10
    returning
      value(E_TIME) type FSEDZ .
  methods SET_IDAPN .
  methods GET_IDAPN
    returning
      value(E_IDAPN) type ZPMED004 .
  methods SET_VORNR
    importing
      !I_ORDEM type AUFNR optional
      !I_VORNR type VORNR optional .
  methods GET_VORNR
    returning
      value(E_VORNR) type VORNR .
  methods CHECK_VORNR
    importing
      !I_ORDEM type AUFNR
      !I_VORNR type VORNR
    returning
      value(E_SUBRC) type SY-SUBRC .
  methods CHECK_QTD_ITEM_BLOCO
    importing
      !I_BLOCO type ZPMED005
    returning
      value(E_QTD_ITEM) type INT4 .
  methods LOG_RETORNO
    importing
      !I_ONLINE type CHAR1 optional
      !I_BLOCO type ZPMED005 optional
      !I_IDORD type ZPMED002 optional
      !I_RETURN type BAPIRET2_T
      !I_IDNOT type ZPMED009 optional
      !I_FIELD type FIELDNAME .
  methods CALL_REPORT
    importing
      !I_SEQUEN type STRING
      !I_REPORT type PROGNAME
      !T_ZPPT0015 type ZPPT0015_T optional
      !I_UNAME type UNAME optional .
  methods SET_IDLOG .
  methods GET_IDLOG
    returning
      value(E_IDLOG) type ZPMED008 .
  methods SERVER
    importing
      !VIEW type RS38L_FNAM optional
    changing
      !REQUEST type ref to IF_HTTP_REQUEST
      !RESPONSE type ref to IF_HTTP_RESPONSE
    returning
      value(DATA) type STRING .
  methods DELETE_OPERACAO
    importing
      !I_ORDEM type AUFNR
    changing
      !I_OPERACAO type ZPMT0017_T .
  methods CRIAR_NOTAS
    importing
      !I_NOTAS type ZPMT0014
      !T_CAUSAS type ZPMT0019_T
      !I_ONLINE type CHAR1 optional
    exporting
      !E_NOTA type QMNUM
      !E_MSG type BAPI_MSG .
  methods GET_IDNOT
    returning
      value(E_IDNOT) type ZPMED009 .
  methods SET_IDILG .
  methods GET_IDILG
    returning
      value(E_IDILG) type ZPMED011 .
  methods STATUS_READ
    importing
      !I_AUFNR type AUFNR optional
      !I_EQUNR type EQUNR optional
    returning
      value(E_LIBERADO) type CHAR1 .
  methods GET_IDVNT
    returning
      value(E_IDVNT) type ZPMED012 .
  methods MODIFY_ORDEM
    importing
      !I_ORDEM type AUFNR
      !I_NOTA type QMNUM
    returning
      value(E_RETURN) type BAPIRET2_T .
  methods VERIFICA_ERROS
    importing
      !I_ORDEM type ZPMT0016_T optional
      !I_OPERACAO type ZPMT0017_T optional
    returning
      value(I_ERROS) type BAPIRET2_T .
  methods SET_UNAME
    importing
      !I_PERNR type PERNR_D .
  methods GET_UNAME
    returning
      value(E_UNAME) type SY-UNAME .
  methods UPDATE_READ_TEXT_ORDEM
    importing
      !I_ORDEM type AUFNR
      !WS_ORDEM type ZPMT0016 .
  PROTECTED SECTION.
private section.

  data T_RETURN type BAPIRET2_T .
  data AT_ORDERID type AUFNR value 1 ##NO_TEXT.
  data AT_REFNUM type IFREFNUM value 1 ##NO_TEXT.
  data AT_SEQOPR type IFREFNUM .
  data AT_ACTIVITY type VORNR .
  data AT_OPER_NO type OBJIDEXT .
  data AT_IDORD type ZPMED002 .
  data AT_IDOPR type ZPMED003 .
  data AT_IDLOG type ZPMED008 .
  data AT_IDAPN type ZPMED005 .
  data AT_VORNR type VORNR .
  data LOG type ZPMT0018_T .
  data AT_CHECK type CHAR1 .
  data AT_IDILG type ZPMED011 .
  data AT_UNAME type SY-UNAME .
ENDCLASS.



CLASS ZCL_PM_ORDEM IMPLEMENTATION.


  METHOD call_report.

    DATA: number           TYPE tbtcjob-jobcount,
          name             TYPE tbtcjob-jobname,
          i_charg          TYPE RANGE OF mchb-charg,
          print_parameters TYPE pri_params.

    name = |JOB_{ i_report }_{ i_sequen }|.

    FREE i_charg.
    i_charg = VALUE #( FOR ls IN t_zppt0015 ( sign = 'I' option = 'EQ' low = ls-lote_individual ) ).

    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = name
      IMPORTING
        jobcount         = number
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.

    IF sy-subrc IS INITIAL.

      IF i_uname IS INITIAL.
        SUBMIT (i_report) WITH bloco = i_sequen
                          WITH charg IN i_charg
                          TO SAP-SPOOL
                          SPOOL PARAMETERS print_parameters
                          WITHOUT SPOOL DYNPRO
                          VIA JOB name NUMBER number
                          AND RETURN.
      ELSE.
        SUBMIT (i_report) WITH bloco = i_sequen
                          WITH charg IN i_charg
                          TO SAP-SPOOL
                          SPOOL PARAMETERS print_parameters
                          WITHOUT SPOOL DYNPRO
                          VIA JOB name NUMBER number
                          USER i_uname
                          AND RETURN.
      ENDIF.

      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'JOB_CLOSE'
          EXPORTING
            jobcount             = number
            jobname              = name
            strtimmed            = abap_true
          EXCEPTIONS
            cant_start_immediate = 1
            invalid_startdate    = 2
            jobname_missing      = 3
            job_close_failed     = 4
            job_nosteps          = 5
            job_notex            = 6
            lock_failed          = 7
            OTHERS               = 8.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD CHECK_QTD_ITEM_BLOCO.
    SELECT COUNT(*) FROM ZPMT0016 INTO E_QTD_ITEM WHERE BLOCO EQ I_BLOCO.
  ENDMETHOD.


  METHOD CHECK_VORNR.
    SELECT COUNT(*)
      FROM AFKO AS A
      INNER JOIN AFVC AS B ON A~AUFPL EQ B~AUFPL
      WHERE A~AUFNR EQ I_ORDEM
        AND B~VORNR EQ I_VORNR.
    E_SUBRC = SY-SUBRC.
  ENDMETHOD.


  METHOD CONVERT_DATE.
    DATA(X_DATA) = I_DATA.

    REPLACE ALL OCCURRENCES OF '/' IN X_DATA WITH ''.
    REPLACE ALL OCCURRENCES OF '.' IN X_DATA WITH ''.

    IF I_VERSAO IS INITIAL.
      TRY.
          CL_ABAP_DATFM=>CONV_DATE_EXT_TO_INT( EXPORTING IM_DATEXT = X_DATA IMPORTING EX_DATINT = E_DATA ).
        CATCH CX_ABAP_DATFM_NO_DATE.
        CATCH CX_ABAP_DATFM_INVALID_DATE.
        CATCH CX_ABAP_DATFM_FORMAT_UNKNOWN.
        CATCH CX_ABAP_DATFM_AMBIGUOUS.
      ENDTRY.
    ELSE.
      E_DATA = X_DATA.
    ENDIF.
  ENDMETHOD.


  METHOD CONVERT_TIME.
    DATA(X_TIME) = I_TIME.

    REPLACE ALL OCCURRENCES OF ':' IN X_TIME WITH ''.
    REPLACE ALL OCCURRENCES OF '.' IN X_TIME WITH ''.

    DATA(STR) = STRLEN( X_TIME ).

    IF STR < 6.
      DO.
        X_TIME+STR(1) = '0'.
        ADD 1 TO STR.
        IF STR EQ 6.
          EXIT.
        ENDIF.
      ENDDO.
      E_TIME = X_TIME.
    ELSEIF STR EQ 6.
      E_TIME = X_TIME.
    ELSEIF STR > 6.
      E_TIME = X_TIME(6).
    ENDIF.

  ENDMETHOD.


  METHOD criar_notas.

    TYPES: BEGIN OF ytxt,
             txt(132),
           END OF ytxt.

    DATA t_txt TYPE TABLE OF ytxt.

    DATA:
*         "// Tabelas de Modificação
      mitem   TYPE TABLE OF bapi2080_notitemi,
      mitem_x TYPE TABLE OF bapi2080_notitemi_x,
      mcaus   TYPE TABLE OF bapi2080_notcausi,
      mcaus_x TYPE TABLE OF bapi2080_notcausi_x,
      mfact   TYPE TABLE OF bapi2080_notactvi,
      mfact_x TYPE TABLE OF bapi2080_notactvi_x,

      ditem   TYPE TABLE OF bapi2080_notitemi,
      dcaus   TYPE TABLE OF bapi2080_notcausi,
      dfact   TYPE TABLE OF bapi2080_notactvi,

      aitem   TYPE TABLE OF bapi2080_notitemi,
      acaus   TYPE TABLE OF bapi2080_notcausi,
      afact   TYPE TABLE OF bapi2080_notactvi,

*     "// Tabela de Recuperação
      gitem   TYPE TABLE OF bapi2080_notiteme,
      gcaus   TYPE TABLE OF bapi2080_notcause,
      gfact   TYPE TABLE OF bapi2080_notactve,

      txtne   TYPE TABLE OF bapi2080_notfulltxte,
      txtni   TYPE TABLE OF bapi2080_notfulltxti.


    DATA: t_notitem           TYPE alm_me_bapi2080_notitemi_t,
          t_notitem_del       TYPE alm_me_bapi2080_notitemi_t,
          t_notitem_x         TYPE eaml_bapi2080_notitemi_x_t,
          t_notifcaus         TYPE alm_me_bapi2080_notcausi_t,
          t_notifcaus_del     TYPE alm_me_bapi2080_notcausi_t,
          t_notifcaus_x       TYPE alm_me_bapi2080_notcausi_x_t,
          t_notifactv         TYPE alm_me_bapi2080_notactvi_t,
          t_notifactv_del     TYPE alm_me_bapi2080_notactvi_t,
          t_notifactv_x       TYPE alm_me_bapi2080_notactvi_x_t,
          lt_item_mod         TYPE TABLE OF bapi2080_notitemi,
          lt_item_modx        TYPE eaml_bapi2080_notitemi_x_t,
          lt_caus_mod         TYPE TABLE OF bapi2080_notcausi,
          lt_caus_modx        TYPE alm_me_bapi2080_notcausi_x_t,
          lt_fact_mod         TYPE TABLE OF bapi2080_notactvi,
          lt_fact_modx        TYPE alm_me_bapi2080_notactvi_x_t,
          _notifheader_export TYPE bapi2080_nothdre,
          t_return            TYPE bapiret2_t,
          _return             TYPE bapiret2_t,
          lv_nota             TYPE bapi2080_nothdre-notif_no,
          lt_zpmt0077         TYPE TABLE OF zpmt0077,
          lv_seq              TYPE sy-tabix.

    DATA(_notifheader) =
    VALUE bapi2080_nothdri(
                            funct_loc    = i_notas-tplnr
                            equipment    = i_notas-equnr
                            short_text   = i_notas-qmtxt
                            code_group   = i_notas-qmgrp
                            coding       = i_notas-qmcod
                            maintplant   = i_notas-iwerk
                            planplant    = i_notas-iwerk
*                            pm_wkctr     = i_notas-arbpl
                            strmlfndate  = i_notas-ausvn
                            strmlfntime  = i_notas-auztv
                            breakdown    = i_notas-msaus
*** Inicio- Rubenilson Pereira - 28.07.2022 - US83600 - MOBMAN
                            plangroup    = i_notas-ingrp
                            desstdate    = i_notas-strmn
                            dessttime    = i_notas-strur
                            desenddate   = i_notas-ltrmn
                            desendtm     = i_notas-ltrur
                            endmlfndate  = i_notas-ausbs
                            endmlfntime  = i_notas-auztb
                            reportedby   = i_notas-qmnam
                            priority     = i_notas-priok

*** Fim- Rubenilson Pereira - 28.07.2022 - US83600 - MOBMAN
                           ).

    SELECT objid
      FROM crhd
      INTO _notifheader-pm_wkctr
      UP TO 1 ROWS
      WHERE arbpl = i_notas-arbpl
        AND werks = i_notas-iwerk.
    ENDSELECT.

    DATA(_notifheader_x) =
    VALUE bapi2080_nothdri_x(
                              funct_loc    = abap_true
                              equipment    = abap_true
                              short_text   = abap_true
                              code_group   = abap_true
                              coding       = abap_true
                              maintplant   = abap_true
                              planplant    = abap_true
                              pm_wkctr     = abap_true
                              strmlfndate  = abap_true
                              strmlfntime  = abap_true
                              breakdown    = abap_true
*** Inicio- Rubenilson Pereira - 28.07.2022 - US83600 - MOBMAN
                              plangroup    = abap_true
                              desstdate    = abap_true
                              dessttime    = abap_true
                              desenddate   = abap_true
                              desendtm     = abap_true
                              endmlfndate  = abap_true
                              endmlfntime  = abap_true
                              reportedby   = abap_true
                              priority     = abap_true
*** Fim- Rubenilson Pereira - 28.07.2022 - US83600 - MOBMAN
    ).

    REFRESH: t_notitem     ,
             t_notitem_del  ,
             t_notitem_x    ,
             t_notifcaus    ,
             t_notifcaus_del,
             t_notifcaus_x  ,
             t_notifactv    ,
             t_notifactv_del,
             t_notifactv_x  ,
             lt_item_mod    ,
             lt_caus_mod    ,
             lt_fact_mod    .

    LOOP AT t_causas INTO DATA(_item) WHERE idnot EQ i_notas-idnot.

      DATA(_item_key) = sy-tabix.

      IF i_notas-qmart EQ 'Y3'.

        IF _item-fenum > '0000'.

          IF _item-eliminado EQ '1'.

            APPEND
            VALUE #(
                      act_key     = _item-fenum
                      act_sort_no = _item-fenum
                      act_codegrp = _item-mngrp
                      act_code    = _item-mncod
            ) TO t_notifactv_del.

          ELSE.

            APPEND
            VALUE #(
            act_key     = _item-fenum
            act_sort_no = _item-fenum
            act_codegrp = _item-mngrp
            act_code    = _item-mncod
            ) TO lt_fact_mod.

            APPEND
            VALUE #(
            act_key     = _item-fenum
            act_sort_no = _item-fenum
            act_codegrp = abap_true
            act_code    = abap_true
            ) TO lt_fact_modx.

          ENDIF.


        ELSE.

          APPEND
          VALUE #(
              act_key     = _item_key
              act_sort_no = _item_key
              act_codegrp = _item-mngrp
              act_code    = _item-mncod
              ) TO t_notifactv.

        ENDIF.


      ELSE.

        IF _item-fenum > '0000'.

          IF _item-eliminado EQ '1'.
            APPEND
                     VALUE #(
                               item_key     = _item-fenum
                               item_sort_no = _item-fenum
                               dl_codegrp   = _item-otgrp
                               dl_code      = _item-oteil
                               d_codegrp    = _item-fegrp
                               d_code       = _item-fecod
                               descript     = _item-fetxt
                     ) TO t_notitem_del.

            APPEND
            VALUE #(
                      cause_key     = '0001'
                      cause_sort_no = _item-fenum
                      item_key      = _item-fenum
                      item_sort_no  = _item-fenum
                      cause_codegrp = _item-urgrp
                      cause_code    = _item-urcod
                      causetext     = _item-urstx
            ) TO t_notifcaus_del.

          ELSE.

            APPEND
            VALUE #(
                      item_key     = _item-fenum
                      item_sort_no = _item-fenum
                      dl_codegrp   = _item-otgrp
                      dl_code      = _item-oteil
                      d_codegrp    = _item-fegrp
                      d_code       = _item-fecod
                      descript     = _item-fetxt
            ) TO lt_item_mod.

            APPEND
            VALUE #(
                      cause_key     = '0001'
                      cause_sort_no = _item-fenum
                      item_key      = _item-fenum
                      item_sort_no  = _item-fenum
                      cause_codegrp = _item-urgrp
                      cause_code    = _item-urcod
                      causetext     = _item-urstx
            ) TO lt_caus_mod.

            APPEND
            VALUE #(
                      item_key     = _item-fenum
                      item_sort_no = _item-fenum
                      dl_codegrp   = abap_true
                      dl_code      = abap_true
                      d_codegrp    = abap_true
                      d_code       = abap_true
                      descript     = abap_true
            ) TO lt_item_modx.

            APPEND
            VALUE #(
                      cause_key     = '0001'
                      cause_sort_no = abap_true
                      item_key      = _item-fenum
                      item_sort_no  = abap_true
                      cause_codegrp = abap_true
                      cause_code    = abap_true
                      causetext     = abap_true
            ) TO lt_caus_modx.

          ENDIF.

        ELSE.

          APPEND
          VALUE #(
                    item_key     = _item_key
                    item_sort_no = _item_key
                    dl_codegrp   = _item-otgrp
                    dl_code      = _item-oteil
                    d_codegrp    = _item-fegrp
                    d_code       = _item-fecod
                    descript     = _item-fetxt
          ) TO t_notitem.

          APPEND
          VALUE #(
                    cause_key     = '0001'
                    cause_sort_no = _item_key
                    item_key      = _item_key
                    item_sort_no  = _item_key
                    cause_codegrp = _item-urgrp
                    cause_code    = _item-urcod
                    causetext     = _item-urstx
          ) TO t_notifcaus.

        ENDIF.

      ENDIF.

    ENDLOOP.

    IF i_notas-qmnum IS NOT INITIAL.

      CALL FUNCTION 'BAPI_ALM_NOTIF_GET_DETAIL' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          number             = i_notas-qmnum
        IMPORTING
          notifheader_export = _notifheader_export
        TABLES
          notlongtxt         = txtne
          notitem            = gitem
          notifcaus          = gcaus
          notifactv          = gfact
          return             = t_return.

      CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY'
        EXPORTING
          number             = i_notas-qmnum
          notifheader        = _notifheader
          notifheader_x      = _notifheader_x
        IMPORTING
          notifheader_export = _notifheader_export
        TABLES
          notifitem          = lt_item_mod
          notifitem_x        = lt_item_modx
          notifcaus          = lt_caus_mod
          notifcaus_x        = lt_caus_modx
          notifactv          = lt_fact_mod
          notifactv_x        = lt_fact_mod
          return             = t_return.
      IF NOT line_exists( t_return[ type = 'E' ] ).

        ADD 1 TO lv_seq.
        APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING FIELD-SYMBOL(<fs_zpmt0077>).
        <fs_zpmt0077>-aufnr = i_notas-qmnum.
        <fs_zpmt0077>-seq   = lv_seq.
        <fs_zpmt0077>-data = sy-datum.
        <fs_zpmt0077>-hora = sy-uzeit.
        <fs_zpmt0077>-tp_operacao = 'NOTA'.
        <fs_zpmt0077>-observacao =  'Erro na BAPI_ALM_NOTIF_DATA_MODIFY'.

        CALL FUNCTION 'IQS4_SAVE_NOTIFICATION'
          EXPORTING
            i_qmnum            = _notifheader_export-notif_no
            i_commit           = abap_true
            i_wait             = abap_true
            i_refresh_complete = abap_true
          TABLES
            return             = t_return.

      ENDIF.

      ditem = VALUE #( FOR ls4 IN t_notitem_del   ( CORRESPONDING #( ls4 ) ) ).
      dcaus = VALUE #( FOR ls5 IN t_notifcaus_del ( CORRESPONDING #( ls5 ) ) ).
      dfact = VALUE #( FOR ls6 IN t_notifactv_del ( CORRESPONDING #( ls6 ) ) ).

      IF ditem IS NOT INITIAL OR dcaus IS NOT INITIAL OR dfact IS NOT INITIAL.

        REFRESH: t_return.

        CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_DELETE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            number    = i_notas-qmnum
          TABLES
            notitem   = ditem
            notifcaus = dcaus
            notifactv = dfact
            return    = t_return.

        IF NOT line_exists( t_return[ type = 'E' ] ).
          ADD 1 TO lv_seq.
          APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
          <fs_zpmt0077>-aufnr = i_notas-qmnum.
          <fs_zpmt0077>-seq   = lv_seq.
          <fs_zpmt0077>-data = sy-datum.
          <fs_zpmt0077>-hora = sy-uzeit.
          <fs_zpmt0077>-tp_operacao = 'NOTA'.
          <fs_zpmt0077>-observacao =  'Erro na BAPI_ALM_NOTIF_DATA_DELETE'.

          e_nota = _notifheader_export-notif_no.

          CALL FUNCTION 'IQS4_SAVE_NOTIFICATION'
            EXPORTING
              i_qmnum            = _notifheader_export-notif_no
              i_commit           = abap_true
              i_wait             = abap_true
              i_refresh_complete = abap_true
            TABLES
              return             = t_return.

        ELSE.

          REFRESH t_return.

        ENDIF.

      ENDIF.

      SELECT SINGLE *
        FROM qmih
        INTO @DATA(ls_qmih)
        WHERE qmnum = @i_notas-qmnum.
      IF sy-subrc IS INITIAL.

        ls_qmih-btpln = i_notas-btpln.
        ls_qmih-bequi = i_notas-bequi.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = ls_qmih-bequi
          IMPORTING
            output = ls_qmih-bequi.

        CALL FUNCTION 'ISU_DB_QMIH_UPDATE'
          EXPORTING
            x_qmih     = ls_qmih
            x_upd_mode = 'U'.

        CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
      ELSE.

        ADD 1 TO lv_seq.
        APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
        <fs_zpmt0077>-aufnr = i_notas-qmnum.
        <fs_zpmt0077>-seq   = lv_seq.
        <fs_zpmt0077>-data = sy-datum.
        <fs_zpmt0077>-hora = sy-uzeit.
        <fs_zpmt0077>-tp_operacao = 'NOTA'.
        <fs_zpmt0077>-observacao =  'Nenhum dado na QMIH'.

        APPEND LINES OF t_return TO _return.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      LOOP AT txtne ASSIGNING FIELD-SYMBOL(<f_txtne>).
        <f_txtne>-objkey = sy-tabix.
      ENDLOOP.

      DATA(v_texto) = i_notas-txtnt.
      DATA(v_tamanho) = strlen( v_texto ).
      WHILE v_tamanho > 1.
        APPEND INITIAL LINE TO t_txt[] ASSIGNING FIELD-SYMBOL(<fs_txt>).

        IF v_tamanho <= 132.
          <fs_txt>-txt = v_texto.
          EXIT.
        ELSE.
          <fs_txt>-txt = v_texto+0(132).
        ENDIF.

        REPLACE ALL OCCURRENCES OF <fs_txt>-txt IN v_texto WITH ''.
        v_tamanho = strlen( v_texto ).
      ENDWHILE.

      txtni = VALUE #(
      FOR ls IN t_txt (
                        objtype    = 'QMEL'
                        objkey     = '00000000'
                        format_col = 'X'
                        text_line  = ls-txt
                       )
                      ).

      aitem = VALUE #( FOR ls1 IN t_notitem  ( CORRESPONDING #( ls1 ) ) ).
      acaus = VALUE #( FOR ls2 IN t_notifcaus ( CORRESPONDING #( ls2 ) ) ).
      afact = VALUE #( FOR ls3 IN t_notifactv ( CORRESPONDING #( ls3 ) ) ).

      TRY .

          IF aitem IS NOT INITIAL OR acaus IS NOT INITIAL OR afact IS NOT INITIAL OR txtni IS NOT INITIAL.

            CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_ADD' "#EC CI_USAGE_OK[2438131]
              EXPORTING
                number             = i_notas-qmnum
                notifheader        = _notifheader
              IMPORTING
                notifheader_export = _notifheader_export
              TABLES
                notfulltxt         = txtni
                notitem            = aitem
                notifcaus          = acaus
                notifactv          = afact
                return             = t_return.

            APPEND LINES OF t_return TO _return.

            IF NOT line_exists( t_return[ type = 'E' ] ).

              ADD 1 TO lv_seq.
              APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
              <fs_zpmt0077>-aufnr = i_notas-qmnum.
              <fs_zpmt0077>-seq   = lv_seq.
              <fs_zpmt0077>-data = sy-datum.
              <fs_zpmt0077>-hora = sy-uzeit.
              <fs_zpmt0077>-tp_operacao = 'NOTA'.
              <fs_zpmt0077>-observacao =  'sucesso na BAPI_ALM_NOTIF_DATA_ADD'.

              e_nota = _notifheader_export-notif_no.

              CALL FUNCTION 'IQS4_SAVE_NOTIFICATION'
                EXPORTING
                  i_qmnum            = _notifheader_export-notif_no
                  i_commit           = abap_true
                  i_wait             = abap_true
                  i_refresh_complete = abap_true
                TABLES
                  return             = t_return.

              APPEND VALUE #( type = 'S' message = |Nota atualizada com Sucesso { _notifheader_export-notif_no }| ) TO _return.

              APPEND LINES OF t_return TO _return.

            ELSE.
              ADD 1 TO lv_seq.
              APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
              <fs_zpmt0077>-aufnr = i_notas-qmnum.
              <fs_zpmt0077>-seq = lv_seq.
              <fs_zpmt0077>-data = sy-datum.
              <fs_zpmt0077>-hora = sy-uzeit.
              <fs_zpmt0077>-tp_operacao = 'NOTA'.
              <fs_zpmt0077>-observacao =  'Erro na BAPI_ALM_NOTIF_DATA_ADD'.
              APPEND LINES OF t_return TO _return.
              CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
            ENDIF.

          ENDIF.

        CATCH cx_root INTO DATA(excp).
          DATA(etext) = excp->if_message~get_text( ).
          APPEND VALUE #( type = 'E' message = etext ) TO _return.
      ENDTRY.

      e_nota = _notifheader_export-notif_no.
      IF _return IS INITIAL.
        CONCATENATE 'Nota' e_nota 'atualizada com sucesso!' INTO e_msg SEPARATED BY space.
      ENDIF.

    ELSE.

      v_texto = i_notas-txtnt.
      v_tamanho = strlen( v_texto ).
      WHILE v_tamanho > 1.
        APPEND INITIAL LINE TO t_txt[] ASSIGNING <fs_txt>.

        IF v_tamanho <= 132.
          <fs_txt>-txt = v_texto.
          exit.
        ELSE.
          <fs_txt>-txt = v_texto+0(132).
        ENDIF.

        REPLACE ALL OCCURRENCES OF <fs_txt>-txt IN v_texto WITH ''.
        v_tamanho = strlen( v_texto ).
      ENDWHILE.
      "ENDLOOP.

*      SPLIT I_NOTAS-TXTNT AT '-->' INTO TABLE T_TXT.
*      DELETE T_TXT WHERE TXT IS INITIAL.

      txtni = VALUE #(
      FOR ls IN t_txt (
                        objtype    = 'QMEL'
                        objkey     = '00000000'
                        format_col = 'X'
                        text_line  = ls-txt
                       )
                      ).

      CALL FUNCTION 'BAPI_ALM_NOTIF_CREATE' "#EC CI_USAGE_OK[2438131]
        EXPORTING
          notif_type         = i_notas-qmart
          notifheader        = _notifheader
        IMPORTING
          notifheader_export = _notifheader_export
        TABLES
          longtexts          = txtni
          notitem            = t_notitem
          notifcaus          = t_notifcaus
          notifactv          = t_notifactv
          return             = t_return.

      APPEND LINES OF t_return TO _return.

      IF NOT line_exists( t_return[ type = 'E' ] ).
        FREE: t_return.
        ADD 1 TO lv_seq.
        APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
        <fs_zpmt0077>-aufnr = i_notas-qmnum.
        <fs_zpmt0077>-seq   = lv_seq.
        <fs_zpmt0077>-data = sy-datum.
        <fs_zpmt0077>-hora = sy-uzeit.
        <fs_zpmt0077>-tp_operacao = 'NOTA'.
        <fs_zpmt0077>-observacao =  'Sucesso  na BAPI_ALM_NOTIF_CREATE'.

        CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE' "#EC CI_USAGE_OK[2438131]
          EXPORTING
            number      = _notifheader_export-notif_no
          IMPORTING
            notifheader = _notifheader_export
          TABLES
            return      = t_return.

        APPEND VALUE #( type = 'S' message = |Nota Criada com Sucesso { _notifheader_export-notif_no }| ) TO _return.
        APPEND LINES OF t_return TO _return.

        IF NOT line_exists( t_return[ type = 'E' ] ).
          ADD 1 TO lv_seq.
          APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
          <fs_zpmt0077>-aufnr = i_notas-qmnum.
          <fs_zpmt0077>-seq   = lv_seq.
          <fs_zpmt0077>-data = sy-datum.
          <fs_zpmt0077>-hora = sy-uzeit.
          <fs_zpmt0077>-tp_operacao = 'NOTA'.
          <fs_zpmt0077>-observacao =  'Sucesso  na BAPI_ALM_NOTIF_SAVE'.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = abap_true.

          e_nota = _notifheader_export-notif_no.

          SELECT SINGLE *
            FROM qmih
            INTO ls_qmih
            WHERE qmnum = e_nota.
          IF sy-subrc IS INITIAL.

            ls_qmih-btpln = i_notas-btpln.
            ls_qmih-bequi = i_notas-bequi.

            CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
              EXPORTING
                input  = ls_qmih-bequi
              IMPORTING
                output = ls_qmih-bequi.

            CALL FUNCTION 'ISU_DB_QMIH_UPDATE'
              EXPORTING
                x_qmih     = ls_qmih
                x_upd_mode = 'U'.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

          ENDIF.

        ELSE.
          ADD 1 TO lv_seq.
          APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
          <fs_zpmt0077>-aufnr = i_notas-qmnum.
          <fs_zpmt0077>-seq   = lv_seq.
          <fs_zpmt0077>-data = sy-datum.
          <fs_zpmt0077>-hora = sy-uzeit.
          <fs_zpmt0077>-tp_operacao = 'NOTA'.
          <fs_zpmt0077>-observacao =  'Erro  na BAPI_ALM_NOTIF_SAVE'.

          APPEND LINES OF t_return TO _return.
          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ENDIF.
      ELSE.
        ADD 1 TO lv_seq.
        APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
        <fs_zpmt0077>-aufnr = i_notas-qmnum.
        <fs_zpmt0077>-seq   = lv_seq.
        <fs_zpmt0077>-data = sy-datum.
        <fs_zpmt0077>-hora = sy-uzeit.
        <fs_zpmt0077>-tp_operacao = 'NOTA'.
        <fs_zpmt0077>-observacao =  'Erro  na BAPI_ALM_NOTIF_CREATE'.

        APPEND LINES OF t_return TO _return.
        CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      ENDIF.

      e_nota = _notifheader_export-notif_no.

    ENDIF.

    IF NOT line_exists( _return[ type = 'E' ] ).

      CALL METHOD modify_ordem
        EXPORTING
          i_ordem  = |{ i_notas-aufnr ALPHA = IN }|
          i_nota   = e_nota
        RECEIVING
          e_return = t_return.

      APPEND LINES OF t_return TO _return.

    ENDIF.

    LOOP AT _return INTO DATA(wa_ret).
      e_msg = COND #( WHEN e_msg IS INITIAL THEN wa_ret-message ELSE |{ e_msg }, { wa_ret-message }| ).
    ENDLOOP.

    IF e_msg IS INITIAL.
      ADD 1 TO lv_seq.
      APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
      <fs_zpmt0077>-aufnr = i_notas-qmnum.
      <fs_zpmt0077>-seq   = lv_seq.
      <fs_zpmt0077>-data = sy-datum.
      <fs_zpmt0077>-hora = sy-uzeit.
      <fs_zpmt0077>-tp_operacao = 'NOTA'.
      <fs_zpmt0077>-observacao =  'Mensagem vazia'.
    ENDIF.

    CALL METHOD me->log_retorno
      EXPORTING
        i_field  = 'NOTA'
        i_online = i_online
        i_idnot  = i_notas-idnot
        i_return = _return.

    IF lt_zpmt0077 IS NOT INITIAL.
      MODIFY zpmt0077 FROM TABLE lt_zpmt0077.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD criar_ordens.

*  "//                                                                                      //"
*  "// Processamento da BAPI de Ordem                                                       //"
*  "// se o campo I_ORDEM-AUFNR estiver preenchido Vamos executar uma Atualização na Ordem  //"
*  "// se o campo I_ORDEM-AUFNR estiver vazio vamos executar uma Criação na Ordem           //"
*  "// as Operações Podem ser Criada com Varias VORNR ou Itens como preferir                //"
*  "// os Apontamentos so poderão ser executado quando a Ordem estiver Liberada             //"
*  "//                                                                                      //"

    DATA t_operation   TYPE bapi_alm_order_operation_t.
    DATA t_objectlist  TYPE bapi_alm_order_objectlist_t.
    DATA t_apontamento TYPE bapi_alm_timeconfirmation_t.
    DATA t_alm_return  TYPE TABLE OF bapi_alm_return.
    DATA t_text        TYPE TABLE OF bapi_alm_text.
    DATA t_text_lines  TYPE TABLE OF bapi_alm_text_lines.
    DATA t_tab_text    TYPE TABLE OF string.
    DATA w_tab_text    TYPE string.
    DATA _return       TYPE bapiret2.
    DATA _method       TYPE swo_method.
    DATA it_operacao   TYPE zpmt0017_t.
    DATA: s_ordem         TYPE aufnr,
          lw_return       TYPE bapiret2,
          lt_permit       TYPE TABLE OF bapi_alm_order_permit,
          lw_csevpermit   TYPE csevpermit,
          lv_objnr        TYPE imprm_objnr_type,
          lt_proptab      TYPE imprm_proptab_type,
          lv_activity     TYPE vornr,
          lt_olist        TYPE TABLE OF bapi_alm_olist_relation,
          ls_header       TYPE bapi_alm_order_header_e,
          lt_return       TYPE TABLE OF bapiret2,
          lt_oprol        TYPE TABLE OF ioprol,
          lv_objnr2       TYPE caufvd-objnr,
          lt_oprol2       TYPE TABLE OF ioprol,
          lv_obknr        TYPE objk-obknr,
          lv_obzae        TYPE objk-obzae,
          lt_oprol3       TYPE TABLE OF bapi_alm_olist_relation,
          ls_header_notif TYPE bapi2080_nothdre,
          ls_caufvd       TYPE caufvd,
          lt_riwol        TYPE TABLE OF riwol,
          g_call_by_iw28  TYPE c,
          lv_oper_auto    TYPE c,
          lt_zpmt0077     TYPE TABLE OF zpmt0077,
          lv_seq          TYPE sy-tabix.

    CLEAR: e_ordem, at_seqopr.

    FREE: t_text,
          t_text_lines,
          t_tab_text.

    at_orderid = |{ at_orderid ALPHA = IN }|.

    IF i_notas[] IS NOT INITIAL.

      TRY .
          DATA(pr_nota) = i_notas[ 1 ]-qmnum.
        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

    ELSE.

      pr_nota = i_ordem-qmnum.

    ENDIF.

    IF i_ordem-aufnr IS NOT INITIAL.
      at_orderid = i_ordem-aufnr.
      _method = 'CHANGE'.
    ELSE.
      at_orderid+0(1) = '%'.
      _method = 'CREATE'.
    ENDIF.

    at_refnum  = |{ at_refnum  ALPHA = IN }|.

    at_oper_no = at_orderid.

    DATA(t_methods) =
    VALUE bapi_alm_order_method_t(
                                    ( refnumber = at_refnum objecttype = 'HEADER'    method = _method  objectkey = at_orderid )
                                 ).

    DATA(t_header) =
    VALUE bapi_alm_order_header_t( (
        orderid      = at_orderid
        order_type   = i_ordem-auart
        funct_loc    = i_ordem-tplnr
*       short_text   = i_ordem-ktext
        planplant    = i_ordem-iwerk
        bus_area     = i_ordem-iwerk
        mn_wk_ctr    = i_ordem-arbpl
        plant        = i_ordem-iwerk
        maintplant   = i_ordem-iwerk
        loc_bus_area = i_ordem-iwerk
        plangroup    = i_ordem-ingpr
        start_date   = i_ordem-dtini
        finish_date  = i_ordem-dtfim
        basicstart   = i_ordem-hrini
        basic_fin    = i_ordem-hrfim
        pmacttype    = i_ordem-ilart
        equipment    = |{ i_ordem-equnr ALPHA = IN }|
        priority     = i_ordem-priok
        notif_no     = pr_nota
        estimated_costs = i_ordem-user4
    ) ).


    SELECT *
      FROM t350
      INTO @DATA(ls_t350)
      UP TO 1 ROWS
      WHERE auart = @i_ordem-auart.
    ENDSELECT.
    IF sy-subrc IS INITIAL.
      IF ls_t350-lngtxt IS NOT INITIAL AND ls_t350-notdat IS NOT INITIAL AND ls_t350-extended_ol EQ '2' .

        lv_oper_auto = abap_true.

        IF _method EQ 'CREATE'.

          READ TABLE i_notas ASSIGNING FIELD-SYMBOL(<fs_nota>) INDEX 1.
          IF sy-subrc IS INITIAL.
            DATA(tabix) = sy-tabix.
            APPEND VALUE #( refnumber = tabix objecttype = 'OBJECTLIST' method = 'CREATE'  objectkey = at_oper_no ) TO t_methods.
            APPEND
                  VALUE #(
                           counter = tabix notif_no = <fs_nota>-qmnum
                         ) TO t_objectlist.
          ENDIF.
        ENDIF.

      ELSE.

        LOOP AT i_notas INTO DATA(_nota).
          tabix = sy-tabix.

          APPEND VALUE #( refnumber = tabix objecttype = 'OBJECTLIST' method = 'CREATE'  objectkey = at_oper_no ) TO t_methods.

          APPEND
          VALUE #(
                   counter = tabix notif_no = _nota-qmnum
                 ) TO t_objectlist.

        ENDLOOP.

      ENDIF.

    ENDIF.

    IF ( lv_oper_auto IS NOT INITIAL AND _method EQ 'CREATE' ) OR lv_oper_auto IS INITIAL.

      it_operacao = i_operacao.

      CALL METHOD me->delete_operacao
        EXPORTING
          i_ordem    = |{ i_ordem-aufnr ALPHA = IN }|
        CHANGING
          i_operacao = it_operacao.

      CLEAR at_seqopr.

      LOOP AT it_operacao INTO DATA(w_operacao).

        DATA(_method_aux) = _method.

        ADD 1 TO at_seqopr.
        at_oper_no+12(4) = w_operacao-vornr.

        IF i_ordem-aufnr IS NOT INITIAL.
          IF me->check_vornr( i_ordem = at_orderid i_vornr = w_operacao-vornr ) IS NOT INITIAL.
            _method_aux = 'CREATE'.
          ENDIF.
        ENDIF.

"FF - 19/04/24 - #131818 - inicio
        if w_operacao-qmnum is NOT INITIAL or i_ordem-QMNUM is not INITIAL.
          _method_aux = 'CHANGE'.
        endif.
"FF - fim


*      IF W_OPERACAO-PHFLG IS NOT INITIAL. --Comentado para não deletar operações através do Aplicativo de Amaggi Manutenção - AOENNING.
*        _METHOD_AUX = 'DELETE'.
*      ENDIF.

        APPEND VALUE #( refnumber = at_seqopr objecttype = 'OPERATION' method = _method_aux  objectkey = at_oper_no ) TO t_methods.

        APPEND VALUE #(
            activity      = w_operacao-vornr
            work_cntr     = w_operacao-arbpl
            control_key   = w_operacao-steus
            plant         = w_operacao-werks
            pers_no       = w_operacao-pernr
            start_cons    = COND #( WHEN w_operacao-ntanf < sy-datum THEN sy-datum ELSE w_operacao-ntanf )
            work_activity = w_operacao-arbei
            un_work               = w_operacao-arbeh
            number_of_capacities  = w_operacao-anzzl
            duration_normal       = w_operacao-dauno
            duration_normal_unit  = w_operacao-daune
            description           = w_operacao-description
            constraint_type_start = '1'
            fin_constr            = w_operacao-ntend
            constraint_type_finish = '1'
        ) TO t_operation.

      ENDLOOP.

    ENDIF.

    IF me->status_read( i_aufnr = at_orderid ) IS INITIAL.
*      APPEND VALUE #( refnumber = at_refnum objecttype = 'HEADER'    method = 'RELEASE' objectkey = at_orderid ) TO t_methods.
    ENDIF.

    APPEND VALUE #( refnumber = ''        objecttype = ''          method = 'SAVE'    objectkey = at_orderid ) TO t_methods.

*----------------------------------------------
*-- Textos
*----------------------------------------------
    IF i_ordem-text_longo IS NOT INITIAL.

      CALL FUNCTION 'CONVERT_STRING_TO_TABLE'
        EXPORTING
          i_string         = i_ordem-text_longo
          i_tabline_length = 132
        TABLES
          et_table         = t_tab_text.

      DESCRIBE TABLE t_tab_text LINES DATA(l_lines).

      APPEND VALUE #( refnumber   = at_refnum
                      objecttype  = 'TEXT'
                      method      = 'CREATE'
                      objectkey   = at_orderid )
                TO t_methods.

      APPEND VALUE #( orderid     = at_orderid
                      langu       = sy-langu
                      textstart   = 1
                      textend     = l_lines )
                TO t_text.

      LOOP AT t_tab_text INTO w_tab_text.
        APPEND VALUE #( tdformat  = '*'
                        tdline    = w_tab_text )
                  TO t_text_lines.
      ENDLOOP.
    ENDIF.

    MOVE-CORRESPONDING i_ordem TO lw_csevpermit.
    lw_csevpermit-artpr = 'PM'.
    lw_csevpermit-waers = 'BRL'.
    lw_csevpermit-swerk = i_ordem-iwerk.
    lv_objnr = 'TM0000000001OR'.

    CALL FUNCTION 'PERMIT_PROPOSAL_BY_CLASS'
      EXPORTING
        i_objnr                     = lv_objnr
        i_csevpermit                = lw_csevpermit
      IMPORTING
        e_proptab                   = lt_proptab
      EXCEPTIONS
        no_permit                   = 1
        no_permit_class             = 2
        no_object_data              = 3
        incomplete_class_definition = 4
        OTHERS                      = 5.
    IF sy-subrc = 0.
      LOOP AT lt_proptab ASSIGNING FIELD-SYMBOL(<fs_proptab>).
        APPEND INITIAL LINE TO lt_permit ASSIGNING FIELD-SYMBOL(<fs_permit>).

        <fs_permit>-permit = <fs_proptab>-pmsog.
        <fs_permit>-release = <fs_proptab>-k_af.

      ENDLOOP.
    ENDIF.

*----------------------------------------------
*-- Executa BAPI
*----------------------------------------------
    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN' "#EC CI_USAGE_OK[2669857]
      TABLES                                "#EC CI_USAGE_OK[2438131]
        it_methods        = t_methods
        it_header         = t_header
        it_operation      = t_operation
        it_objectlist     = t_objectlist
        it_olist_relation = lt_olist
        it_text           = t_text
        it_text_lines     = t_text_lines
        return            = t_return.
*        it_permit     = lt_permit.

    LOOP AT t_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      ADD 1 TO lv_seq.
      APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING FIELD-SYMBOL(<fs_zpmt0077>).
      <fs_zpmt0077>-aufnr = i_ordem-aufnr.
      <fs_zpmt0077>-seq   = lv_seq.
      <fs_zpmt0077>-data = sy-datum.
      <fs_zpmt0077>-hora = sy-uzeit.
      <fs_zpmt0077>-tp_operacao = 'ORDEM'.
      <fs_zpmt0077>-observacao = <fs_return>-message.
    ENDLOOP.

    IF NOT line_exists( t_return[ type = 'E' ] ).

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          wait = abap_true.

      TRY .
          IF i_ordem-aufnr IS INITIAL.
            LOOP AT t_return INTO DATA(s_return).
              CASE s_return-number.
                WHEN '112' OR '126'.
                  s_ordem = s_return-message_v2.
                  s_ordem = |{ s_ordem ALPHA = IN }|.
*- Não e mais necessário -------
*                 me->update_read_text_ordem( i_ordem =  s_ordem ws_ordem = i_ordem ).
*-------------------------------
                  e_ordem = |{ s_return-message_v2 ALPHA = OUT }|.
              ENDCASE.
            ENDLOOP.
          ELSE.
            e_ordem = |{ t_return[ number = '080' ]-message_v1 ALPHA = OUT }|.

          ENDIF.


        CATCH cx_sy_itab_line_not_found.
      ENDTRY.

      IF e_ordem IS INITIAL.
        e_ordem = i_ordem-aufnr.
      ENDIF.

      IF lv_oper_auto IS NOT INITIAL .

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = e_ordem
          IMPORTING
            output = e_ordem.
*            SELECT *
*              FROM objk
*              INTO TABLE @DATA(lt_obknr)
*              FOR ALL ENTRIES IN @i_notas
*              WHERE ihnum = @i_notas-qmnum.
*            IF sy-subrc IS INITIAL.
*              SORT lt_obknr BY ihnum.
*            ENDIF.

*            CALL FUNCTION 'IOPEXT_ORDER_BAPI_READ'
*              EXPORTING
*                i_aufnr        = e_ordem
*                i_refresh_iwol = 'X'
*                i_refresh_iqs1 = g_call_by_iw28
*                i_refresh_coih = g_call_by_iw28                       "note 653254
*              IMPORTING
*                es_caufvd      = ls_caufvd.
*
*            LOOP AT i_notas ASSIGNING FIELD-SYMBOL(<fs_nota>).
*              CLEAR lt_riwol.
*
*              APPEND INITIAL LINE TO lt_riwol ASSIGNING FIELD-SYMBOL(<fs_riwol>).
*              <fs_riwol>-ihnum = <fs_nota>-qmnum.
*
*            ENDLOOP.
**--- ein Eintrag in der Objektliste
*            CALL FUNCTION 'IWOL_ADD_OBJECTS_TO_DDB_EXT'
*              EXPORTING
*                i_caufvd     = ls_caufvd
*              TABLES
*                t_riwol      = lt_riwol
*              EXCEPTIONS
*                show_message = 1
*                OTHERS       = 2.
*            LOOP AT i_notas ASSIGNING FIELD-SYMBOL(<fs_nota>).
**              ADD 1 TO lv_obzae.
*              IF sy-tabix EQ 1 AND lt_oprol3 IS INITIAL.
*                CONTINUE.
*              ENDIF.

*              READ TABLE lt_obknr ASSIGNING FIELD-SYMBOL(<fs_obknr>)
*              WITH KEY ihnum = <fs_nota>-qmnum
*              BINARY SEARCH.
*              IF sy-subrc IS INITIAL.

        CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
          EXPORTING
            number   = e_ordem
          TABLES
            et_oprol = lt_oprol3
            return   = lt_return.

        CALL FUNCTION 'ZPM_ATUALIZA_OBJETOS_ORDEM' IN BACKGROUND TASK AS SEPARATE UNIT
          EXPORTING
            i_aufnr = e_ordem
            i_notas = i_notas
          TABLES
            i_oprol = lt_oprol3.

*              CALL FUNCTION 'IOPEXT_INSERT_OL_NOTIF'
*                DESTINATION 'NONE'
*                EXPORTING
*                  i_aufnr     = e_ordem
*                  i_qmnum     = <fs_nota>-qmnum
*                  i_obknr_ins = lv_obknr
*                  i_obzae_ins = lv_obzae.

*                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

*              ENDIF.

*            ENDLOOP.

      ENDIF.

*          CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
*            EXPORTING
*              number    = e_ordem
*            IMPORTING
*              es_header = ls_header
*            TABLES
*              return    = lt_return.
*
*          IF ls_header-routing_no IS NOT INITIAL.
*
*            READ TABLE i_notas ASSIGNING FIELD-SYMBOL(<fs_notas>) INDEX 1.
*            IF sy-subrc IS INITIAL.
*
*              READ TABLE lt_obknr ASSIGNING <fs_obknr>
*              WITH KEY ihnum = <fs_nota>-qmnum
*              BINARY SEARCH.
*              IF sy-subrc IS INITIAL.
*
*                CALL FUNCTION 'ALM_ME_NOTIFICATION_GETDETAIL2'
*                  EXPORTING
*                    notif_no            = <fs_notas>-qmnum
*                  IMPORTING
*                    notification_header = ls_header_notif.
*
*                APPEND INITIAL LINE TO lt_oprol ASSIGNING FIELD-SYMBOL(<fs_oprol>).
*                <fs_oprol>-aufpl = ls_header-routing_no.
*                <fs_oprol>-obknr = <fs_obknr>-obknr.
*                <fs_oprol>-aplzl = '00000001'.
*                <fs_oprol>-obzae = '1'.
*                <fs_oprol>-vbkz  = 'I'.
*
*                CALL FUNCTION 'IOPOL_OPROL_SAVE'
*                  TABLES
*                    ioprol_db = lt_oprol.
*
*                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
*                  EXPORTING
*                    wait = 'X'.
*
*                FREE: t_operation,
*                      t_methods.
*
*                APPEND VALUE #( refnumber = at_seqopr objecttype = 'OPERATION' method = 'CHANGE'  objectkey = e_ordem && '0010' ) TO t_methods.
*                APPEND VALUE #( refnumber = '' objecttype = '' method = 'SAVE'  objectkey = e_ordem ) TO t_methods.
*
*                APPEND INITIAL LINE TO t_operation ASSIGNING FIELD-SYMBOL(<fs_operation>).
*
*                <fs_operation>-activity    = '0010'.
*                <fs_operation>-description =  ls_header_notif-short_text.
*
*                CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
*                  TABLES
*                    it_methods   = t_methods
*                    it_operation = t_operation
*                    return       = t_return.
*
*                CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
*
*              ENDIF.
*
*            ENDIF.
*
*          ENDIF.

      IF i_ordem-stato EQ '1'.

        LOOP AT i_apontamento ASSIGNING FIELD-SYMBOL(<fs_apont>).

          IF <fs_apont>-stokz IS NOT INITIAL.

            CALL FUNCTION 'BAPI_ALM_CONF_CANCEL'
              EXPORTING
                confirmation        = <fs_apont>-rueck
                confirmationcounter = <fs_apont>-rmzhl
*               POSTGDATE           =
*               CONFTEXT            =
              IMPORTING
                return              = lw_return
*               LOCKED              =
*               CREATED_CONF_NO     =
*               CREATED_CONF_COUNT  =
              .
            IF lw_return-type <> 'E' .

              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
                EXPORTING
                  wait = 'X'.

              ADD 1 TO lv_seq.
              APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
              <fs_zpmt0077>-aufnr = i_ordem-aufnr.
              <fs_zpmt0077>-seq   = lv_seq.
              <fs_zpmt0077>-data = sy-datum.
              <fs_zpmt0077>-hora = sy-uzeit.
              <fs_zpmt0077>-tp_operacao = 'ORDEM'.
              <fs_zpmt0077>-observacao =  'BAPI_ALM_CONF_CANCEL executada com sucesso'.

            ENDIF.


          ENDIF.

        ENDLOOP.

        t_apontamento =
        VALUE #(
                  FOR ls  IN i_operacao
                  FOR ls1 IN i_apontamento WHERE ( idord EQ ls-idord
                                               AND idopr EQ ls-idopr
                                               AND rueck IS INITIAL )
                         (
                           orderid         = |{ e_ordem ALPHA = IN }|
                           operation       = ls-vornr
                           postg_date      = ls1-budat
                           work_cntr       = ls1-arbpl
                           pers_no         = ls1-pernr
                           exec_start_date = ls1-isdd
                           exec_start_time = ls1-isdz
                           exec_fin_date   = ls1-iedd
                           exec_fin_time   = ls1-iedz
                           fin_conf        = ls1-aueru
                           act_work        = ls1-ismnw
                           un_work         = ls1-ismne
                           dev_reason      = ls1-grund
                         )
              ).

        IF t_apontamento IS NOT INITIAL.

          CALL FUNCTION 'BAPI_ALM_CONF_CREATE'
            IMPORTING
              return        = _return
            TABLES
              timetickets   = t_apontamento
              detail_return = t_alm_return.

          IF _return IS INITIAL.

            ADD 1 TO lv_seq.
            APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
            <fs_zpmt0077>-aufnr = i_ordem-aufnr.
            <fs_zpmt0077>-seq   = lv_seq.
            <fs_zpmt0077>-data = sy-datum.
            <fs_zpmt0077>-hora = sy-uzeit.
            <fs_zpmt0077>-tp_operacao = 'ORDEM'.
            <fs_zpmt0077>-observacao =  'Apontamento realizado com sucesso'.

            CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait = abap_true.

            LOOP AT t_alm_return INTO DATA(_alm_return)..

              APPEND INITIAL LINE TO e_apontamentos ASSIGNING FIELD-SYMBOL(<fs_apontamentos>).

              <fs_apontamentos>-confirmacao = _alm_return-conf_no.
              <fs_apontamentos>-item        = _alm_return-conf_cnt.

              APPEND CORRESPONDING #( _alm_return ) TO t_return.

            ENDLOOP.

          ELSE.

            ADD 1 TO lv_seq.
            APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
            <fs_zpmt0077>-aufnr = i_ordem-aufnr.
            <fs_zpmt0077>-seq   = lv_seq.
            <fs_zpmt0077>-data = sy-datum.
            <fs_zpmt0077>-hora = sy-uzeit.
            <fs_zpmt0077>-tp_operacao = 'ORDEM'.
            <fs_zpmt0077>-observacao =  'Erro no apontamento'.

            APPEND _return TO t_return.

            LOOP AT t_alm_return INTO _alm_return.

              APPEND CORRESPONDING #( _alm_return ) TO t_return.

              APPEND INITIAL LINE TO e_apontamentos ASSIGNING <fs_apontamentos>.
              <fs_apontamentos>-msg_erro = _alm_return-message.

            ENDLOOP.

            CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.

      ADD 1 TO lv_seq.
      APPEND INITIAL LINE TO lt_zpmt0077 ASSIGNING <fs_zpmt0077>.
      <fs_zpmt0077>-aufnr = i_ordem-aufnr.
      <fs_zpmt0077>-seq   = lv_seq.
      <fs_zpmt0077>-data = sy-datum.
      <fs_zpmt0077>-hora = sy-uzeit.
      <fs_zpmt0077>-tp_operacao = 'ORDEM'.
      <fs_zpmt0077>-observacao =  'Erro ao salvar na MAINTAIN'.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      CLEAR e_msg.
      LOOP AT t_return INTO DATA(wa_ret).
        e_msg = COND #( WHEN e_msg IS INITIAL THEN wa_ret-message ELSE |{ e_msg }, { wa_ret-message }| ).
      ENDLOOP.

    ENDIF.

    CALL METHOD me->log_retorno
      EXPORTING
        i_field  = 'ORDEM'
        i_online = i_online
        i_bloco  = i_ordem-bloco
        i_idord  = i_ordem-idord
        i_return = t_return.

    IF lt_zpmt0077 IS NOT INITIAL.
      MODIFY zpmt0077 FROM TABLE lt_zpmt0077.
      IF sy-subrc IS INITIAL.
        COMMIT WORK.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD DELETE_OPERACAO.

    CHECK AT_ORDERID IS NOT INITIAL.

    SELECT B~*
      FROM AFKO AS A
      INNER JOIN AFVC AS B ON A~AUFPL EQ B~AUFPL
      INTO TABLE @DATA(_OPERACOES)
      WHERE A~AUFNR EQ @I_ORDEM
        AND B~PHFLG EQ @ABAP_FALSE.

    LOOP AT _OPERACOES INTO DATA(W_OPE).
      IF NOT LINE_EXISTS( I_OPERACAO[ VORNR = W_OPE-VORNR ] ).
        W_OPE-PHFLG = ABAP_TRUE.
        APPEND CORRESPONDING #( W_OPE ) TO I_OPERACAO.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_BLOCO.
    SELECT MAX( BLOCO ) FROM ZPMT0016 INTO E_BLOCO. "#EC CI_NOWHERE
    ADD 1 TO E_BLOCO.
  ENDMETHOD.


  METHOD GET_IDAPN.
    E_IDAPN = AT_IDAPN.
  ENDMETHOD.


  METHOD GET_IDILG.
    ADD 1 TO AT_IDILG.
    E_IDILG = AT_IDILG.
  ENDMETHOD.


  METHOD GET_IDLOG.
    E_IDLOG = AT_IDLOG.
  ENDMETHOD.


  METHOD GET_IDNOT.
    SELECT MAX( IDNOT ) FROM ZPMT0014 INTO E_IDNOT.
    ADD 1 TO E_IDNOT.
  ENDMETHOD.


  METHOD GET_IDOPR.
    E_IDOPR = AT_IDOPR.
  ENDMETHOD.


  METHOD GET_IDORD.
    E_IDOPR = AT_IDORD.
  ENDMETHOD.


  METHOD GET_IDVNT.
    SELECT MAX( IDVNT ) FROM ZPMT0020 INTO E_IDVNT. "#EC CI_NOWHERE
  ENDMETHOD.


  METHOD GET_UNAME.
    E_UNAME = AT_UNAME.
  ENDMETHOD.


  METHOD GET_VORNR.
      E_VORNR = AT_VORNR.
  ENDMETHOD.


  METHOD LOG_RETORNO.
    CHECK I_ONLINE IS INITIAL.
    CHECK I_RETURN IS NOT INITIAL.

    ME->SET_IDLOG( ).
    ME->SET_IDILG( ).

    LOG = VALUE #(
    FOR L_RET IN I_RETURN (
                              IDLOG   = |{ ME->GET_IDLOG( ) ALPHA = IN }|
                              IDILG   = |{ ME->GET_IDILG( ) ALPHA = IN }|
                              FIELD   = I_FIELD
                              BLOCO   = I_BLOCO
                              IDORD   = I_IDORD
                              IDNOT   = I_IDNOT
                              TYPE    = L_RET-TYPE
                              MESSAGE = L_RET-MESSAGE
                          )
                 ).

    MODIFY ZPMT0018 FROM TABLE LOG.
    COMMIT WORK.
  ENDMETHOD.


  METHOD MODIFY_ORDEM.

    CHECK I_ORDEM IS NOT INITIAL.

    DATA: ESHEADER  TYPE BAPI_ALM_ORDER_HEADER_E,
          IT_RETURN TYPE TABLE OF BAPIRET2,
          IT_OLIST  TYPE TABLE OF BAPI_ALM_ORDER_OBJECTLIST,
          ISHEADER  TYPE TABLE OF BAPI_ALM_ORDER_HEADERS_I.


    CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL' "#EC CI_USAGE_OK[2669857]
      EXPORTING                               "#EC CI_USAGE_OK[2438131]
        NUMBER    = I_ORDEM
      IMPORTING
        ES_HEADER = ESHEADER
      TABLES
        RETURN    = IT_RETURN
        ET_OLIST  = IT_OLIST
      EXCEPTIONS
        OTHERS    = 01.

    DATA(LINES_OBJ) = LINES( IT_OLIST ).
    ADD 1 TO LINES_OBJ.

    APPEND LINES OF IT_RETURN TO E_RETURN.

    DATA(T_METHODS) =
    VALUE BAPI_ALM_ORDER_METHOD_T(
                                    ( REFNUMBER = 1  OBJECTTYPE = 'OBJECTLIST' METHOD = 'CREATE' OBJECTKEY = I_ORDEM )
                                    ( REFNUMBER = '' OBJECTTYPE = ''           METHOD = 'SAVE'   OBJECTKEY = I_ORDEM )
                                 ).

    IF ESHEADER-NOTIF_NO IS INITIAL.
      APPEND VALUE #( REFNUMBER = 1  OBJECTTYPE = 'HEADER'     METHOD = 'CHANGE' OBJECTKEY = I_ORDEM ) TO T_METHODS.

      ESHEADER-NOTIF_NO = I_NOTA.
      APPEND CORRESPONDING #( ESHEADER ) TO ISHEADER.
    ENDIF.

    DATA(T_OBJECTLIST) =
    VALUE BAPI_ALM_ORDER_OBJECTLIST_T( (
            COUNTER = LINES_OBJ NOTIF_NO = I_NOTA
    ) ).

    CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN' "#EC CI_USAGE_OK[2669857]
      TABLES                                "#EC CI_USAGE_OK[2438131]
        IT_METHODS    = T_METHODS
        IT_HEADER     = ISHEADER
        IT_OBJECTLIST = T_OBJECTLIST
        RETURN        = IT_RETURN.

    APPEND LINES OF IT_RETURN TO E_RETURN.

    IF NOT LINE_EXISTS( IT_RETURN[ TYPE = 'E' ] ).
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  METHOD server.

    DATA oexcp        TYPE REF TO cx_root.
    DATA function     TYPE rs38l_fnam.
    DATA exceptheader TYPE string.
    DATA params       TYPE zrfc_fint_p_t.
    DATA paramtab     TYPE abap_func_parmbind_tab.
    DATA exceptab     TYPE abap_func_excpbind_tab.
    DATA open_key     TYPE char1 VALUE '{'.
    DATA close_key    TYPE char1 VALUE '}'.

    DATA(i_cdata) = request->get_cdata( ).

    function = SWITCH #( view
                    WHEN 'CRIAR_NOTA'  THEN 'ZPM_IMPORTA_NOTA_PM'
                    WHEN 'CRIAR_ORDEM' THEN 'ZPM_IMPORTA_ORDEM_PM' ).

    CALL METHOD zcl_fmcall_handler=>build_params
      EXPORTING
        function_name    = function
      IMPORTING
        params           = params
        paramtab         = paramtab
        exceptab         = exceptab
      EXCEPTIONS
        invalid_function = 1
        OTHERS           = 2.

    DATA(_parameter) = params[ paramclass = 'I' exid = 'h' ]-parameter.
    DATA(_cdata) = |{ open_key } "{ _parameter }": { i_cdata } { close_key }|.

    TRY.
        CALL METHOD zcl_fmcall_pm_mobile=>json_deserialize
          EXPORTING
            json     = _cdata
          CHANGING
            paramtab = paramtab.
      CATCH cx_root INTO oexcp.
        DATA(etext) = oexcp->if_message~get_text( ).
    ENDTRY.

    TRY.
        CALL FUNCTION function
          PARAMETER-TABLE
          paramtab
          EXCEPTION-TABLE
          exceptab.
      CATCH cx_root INTO oexcp.
        etext = oexcp->if_message~get_longtext(  preserve_newlines = abap_true ).
    ENDTRY.

    DATA(funcrc) = sy-subrc.
    DELETE exceptab WHERE value NE funcrc.

    IF line_exists( exceptab[ value = funcrc ] ).

      DATA(exception) = exceptab[ value = funcrc ].

      exceptheader = exception-name.

      CALL METHOD response->set_header_field(
          name  = 'X-SAPRFC-Exception'
          value = exceptheader ).

    ENDIF.

    CALL METHOD zcl_fmcall_pm_mobile=>serialize_json
      EXPORTING
        paramtab  = paramtab
        exceptab  = exceptab
        params    = params
        lowercase = abap_true
      IMPORTING
        o_string  = data.

    FIELD-SYMBOLS: <ls_retorno> TYPE ztpm_transa_exp_ordem,
                   <lt_ordens>  TYPE ztpm_d_m_ordem_t,
                   <ls_param>   TYPE any,
                   <fs_sucess>  TYPE c.

    DATA: ls_retorno2 TYPE ztpm_transa_exp_ordem,
          lo_retorno  TYPE REF TO data,
          lo_ordens   TYPE REF TO data,
          lt_ordens   TYPE ztpm_d_m_ordem_t.

    lo_retorno = NEW ztpm_transa_exp_ordem( ).
    lo_ordens  = NEW ztpm_d_m_ordem_t( ).

    READ TABLE paramtab TRANSPORTING NO FIELDS
    WITH KEY name = 'ORDENS'.
    IF sy-subrc IS INITIAL.

      READ TABLE paramtab ASSIGNING FIELD-SYMBOL(<fs_param>)
      WITH KEY name = 'RETORNO'.
      IF sy-subrc IS INITIAL.
        ASSIGN <fs_param>-value TO <ls_param>.
        lo_retorno = <ls_param>.
        ls_retorno2 = CAST ztpm_transa_exp_ordem( lo_retorno )->* .

        IF ls_retorno2-sucess EQ abap_true AND ls_retorno2-data-nr_ordem IS NOT INITIAL.

          DATA(lv_ordem) = ls_retorno2-data-nr_ordem.

          READ TABLE paramtab ASSIGNING <fs_param>
          WITH KEY name = 'ORDENS'.
          IF sy-subrc IS INITIAL.

            ASSIGN <fs_param>-value TO <ls_param>.
            lo_ordens = <ls_param>.
            lt_ordens = CAST ztpm_d_m_ordem_t( lo_ordens )->* .

            READ TABLE lt_ordens ASSIGNING FIELD-SYMBOL(<fs_ordens>) INDEX 1.
            IF sy-subrc IS INITIAL.
              <fs_ordens>-aufnr = lv_ordem.

              CALL FUNCTION 'ZPM_DISPARA_API_ORDEM' IN BACKGROUND TASK
                EXPORTING
                  ordem_orc = lt_ordens.

              COMMIT WORK.

            ENDIF.

          ENDIF.

        ENDIF.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD SET_IDAPN.
    ADD 1 TO AT_IDAPN.
  ENDMETHOD.


  METHOD SET_IDILG.
    AT_IDILG = 0.
  ENDMETHOD.


  METHOD SET_IDLOG.
    SELECT MAX( IDLOG ) "#EC CI_NOWHERE
      FROM ZPMT0018
      INTO AT_IDLOG.
    ADD 1 TO AT_IDLOG.
  ENDMETHOD.


  METHOD SET_IDOPR.
    ADD 1 TO AT_IDOPR.
  ENDMETHOD.


  METHOD SET_IDORD.
    ADD 1 TO AT_IDORD.
    AT_CHECK = ABAP_FALSE.
  ENDMETHOD.


  METHOD SET_UNAME.

    IF I_PERNR IS NOT INITIAL..

      SELECT SINGLE BNAME
       FROM ZHCMT0007
       INTO AT_UNAME
        WHERE PERNR EQ I_PERNR.

      IF SY-SUBRC IS NOT INITIAL..
        AT_UNAME = SY-UNAME.
      ENDIF.
    ELSE.
      AT_UNAME = SY-UNAME.
    ENDIF.

  ENDMETHOD.


  METHOD SET_VORNR.
    IF I_ORDEM IS INITIAL.
      ADD 10 TO AT_VORNR.
    ELSE.
      SELECT SINGLE B~APLZL
          FROM AFKO AS A
            INNER JOIN AFVC AS B ON A~AUFPL EQ B~AUFPL
              INTO @DATA(_APLZL)
              WHERE A~AUFNR EQ @I_ORDEM
                AND B~VORNR EQ @I_VORNR.
      IF SY-SUBRC IS INITIAL.
        AT_VORNR = |{ _APLZL ALPHA = OUT }|.
        MULTIPLY AT_VORNR BY 10.
        AT_VORNR = |{ AT_VORNR ALPHA = IN }|.
      ELSE.
        SELECT MAX( B~APLZL )
            FROM AFKO AS A
              INNER JOIN AFVC AS B ON A~AUFPL EQ B~AUFPL
                INTO _APLZL
                WHERE A~AUFNR EQ I_ORDEM.
        IF SY-SUBRC IS INITIAL.
          IF AT_CHECK IS INITIAL.
            AT_CHECK = ABAP_TRUE.
            ADD 1 TO _APLZL.
            AT_VORNR = |{ _APLZL ALPHA = OUT }|.
            MULTIPLY AT_VORNR BY 10.
            AT_VORNR = |{ AT_VORNR ALPHA = IN }|.
          ELSE.
            ADD 10 TO AT_VORNR.
          ENDIF.
        ELSE.
          ADD 10 TO AT_VORNR.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD STATUS_READ.

    DATA T_STATUS TYPE TABLE OF JSTAT.

    IF I_AUFNR IS NOT INITIAL..

      SELECT SINGLE OBJNR
        FROM AUFK
        INTO @DATA(VL_OBJNR)
        WHERE AUFNR = @I_AUFNR.

    ELSEIF I_EQUNR IS NOT INITIAL.

      SELECT SINGLE OBJNR
        FROM EQUI
        INTO VL_OBJNR
        WHERE EQUNR = I_EQUNR.

    ENDIF.

    CALL FUNCTION 'STATUS_READ'
      EXPORTING
        CLIENT           = SY-MANDT
        OBJNR            = VL_OBJNR
        ONLY_ACTIVE      = ABAP_TRUE
      TABLES
        STATUS           = T_STATUS
      EXCEPTIONS
        OBJECT_NOT_FOUND = 1
        OTHERS           = 2.

    IF SY-SUBRC IS INITIAL.
      IF ( LINE_EXISTS( T_STATUS[ STAT = 'I0002' ] ) ) AND I_AUFNR IS NOT INITIAL.
        E_LIBERADO = ABAP_TRUE.
      ELSEIF ( LINE_EXISTS( T_STATUS[ STAT = 'I0076' ] ) ) AND I_EQUNR IS NOT INITIAL.
        E_LIBERADO = ABAP_TRUE.
      ELSEIF ( LINE_EXISTS( T_STATUS[ STAT = 'I0320' ] ) ) AND I_EQUNR IS NOT INITIAL.
        E_LIBERADO = ABAP_TRUE.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD update_read_text_ordem.
    DATA: t_lines  TYPE TABLE OF tline,
          ti_lines TYPE TABLE OF tline,
          wa_lines LIKE LINE OF ti_lines,
          lv_name  TYPE thead-tdname.

    DATA: lv_aufpl TYPE afko-aufpl,
          v_mandt  TYPE aufk-mandt,
          vl_aufpl TYPE afvc-aufpl,
          v_aufnr  TYPE aufk-aufnr,
          lv_aplzl TYPE afvc-aplzl.

    DATA:
      wa_header TYPE thead.

    CONSTANTS cs_vornr TYPE afvc-vornr VALUE '0010'.
    SELECT SINGLE mandt, aufnr
            FROM aufk
            INTO ( @v_mandt, @v_aufnr )
           WHERE aufnr EQ @i_ordem.


*    IF lv_aufpl IS NOT INITIAL.
*
*      SELECT SINGLE mandt aufpl aplzl
*        FROM afvc
*        INTO ( lv_mandt, vl_aufpl, lv_aplzl )
*       WHERE aufpl = lv_aufpl
*         AND vornr = cs_vornr.
*    ENDIF.

    IF v_mandt IS NOT INITIAL AND
       v_aufnr IS NOT INITIAL.

      CONCATENATE v_mandt v_aufnr INTO lv_name.

      IF ws_ordem-text_longo IS NOT INITIAL.

        DATA(v_texto) = ws_ordem-text_longo.
        DATA(v_tamanho) = strlen( v_texto ).
*        WHILE v_tamanho > 1.
        APPEND INITIAL LINE TO t_lines[] ASSIGNING FIELD-SYMBOL(<fs_txt>).

*          IF v_tamanho <= 70.
*            <fs_txt>-tdline = v_texto.
*          ELSE.
*            <fs_txt>-tdline = v_texto+0(70).
*          ENDIF.
*
*          REPLACE ALL OCCURRENCES OF <fs_txt>-tdline IN v_texto WITH ''.
*          v_tamanho = strlen( v_texto ).
        wa_lines-tdline = v_texto.
        wa_header-tdobject = 'AUFK'.
        wa_header-tdspras = sy-langu.
        wa_header-tdid    = 'KOPF'. "'AVOT'.
        wa_header-tdname  = lv_name.
        APPEND wa_lines TO t_lines.
*        ENDWHILE.
        IF t_lines IS NOT INITIAL.
          DELETE t_lines WHERE  tdline EQ ''.

* Guardar texto
          CALL FUNCTION 'SAVE_TEXT'
            EXPORTING
              client          = sy-mandt
              header          = wa_header
*             insert          = 'X'
              savemode_direct = 'X'
            TABLES
              lines           = t_lines
            EXCEPTIONS
              id              = 1
              language        = 2
              name            = 3
              object          = 4
              OTHERS          = 5.
          IF sy-subrc EQ 0.
            CALL FUNCTION 'COMMIT_TEXT'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    CLEAR: v_mandt, v_aufnr.
*    ENDIF.

  ENDMETHOD.


  METHOD VERIFICA_ERROS.
* "// Regras de negócio - Ordem de manutenção

    LOOP AT I_ORDEM INTO DATA(_ORDEM).

      DATA(_IWERK) = |{ _ORDEM-IWERK ALPHA = IN }|.

      IF _ORDEM-EQUNR IS NOT INITIAL.

        DATA(_EQUNR)  = |{ _ORDEM-EQUNR ALPHA = IN }|.

        SELECT COUNT(*)
        FROM EQUZ
        WHERE EQUNR EQ _EQUNR
          AND IWERK EQ _IWERK
          AND DATBI EQ '99991231'. "// ( Data de validade final )

        IF SY-SUBRC IS NOT INITIAL.
          APPEND VALUE #(
                          TYPE  = 'E'
                          MESSAGE = |Centro de Planejamento da Ordem { _IWERK } é diferente do Centro de Planejamento do Equipamento { _ORDEM-EQUNR } |
                        ) TO I_ERROS.
        ENDIF.

      ELSE."//  (Verifica a tabela de local e compara o centro de planejamento).

        SELECT COUNT(*)
        FROM ILOA
        WHERE TPLNR EQ _ORDEM-TPLNR
          AND SWERK EQ _IWERK.

        IF SY-SUBRC IS NOT INITIAL.
          APPEND VALUE #(
                          TYPE  = 'E'
                          MESSAGE = |Centro de Planejamento da Ordem { _IWERK } é diferente do Centro de Planejamento do Local { _ORDEM-TPLNR } |
                        ) TO I_ERROS.
        ENDIF.
      ENDIF.

*       "//Check centro de trabalho - O centro de trabalho da Ordem tem que existir no centro de planejamento.
      SELECT COUNT(*)
      FROM CRHD
      WHERE ARBPL EQ _ORDEM-ARBPL
        AND WERKS EQ _IWERK.

      IF SY-SUBRC IS NOT INITIAL.
        APPEND VALUE #(
                        TYPE = 'E'
                        MESSAGE = |Centro de Trabalho da Ordem { _ORDEM-ARBPL }/{ _IWERK } não existe!|
                      ) TO I_ERROS.
      ENDIF.

*       "//Check data da operação - A data da operação tem que ser igual ou maior que a data atual.
*      IF _ORDEM-DTINI < SY-DATUM.
*        APPEND VALUE #(
*                        TYPE = 'E'
*                        MESSAGE = |Data de Reinicio está no passado!|
*                      ) TO I_ERROS.
*      ENDIF.

      LOOP AT I_OPERACAO INTO DATA(_OPERACAO) WHERE BLOCO EQ _ORDEM-BLOCO
                                                AND IDORD EQ _ORDEM-IDORD.

        SELECT COUNT(*)
        FROM CRHD
        WHERE ARBPL EQ _OPERACAO-ARBPL
          AND WERKS EQ _IWERK.

        IF SY-SUBRC IS NOT INITIAL.
          APPEND VALUE #(
                          TYPE = 'E'
                          MESSAGE = |Centro de Trabalho da Operação { _OPERACAO-ARBPL }/{ _IWERK } não existe!|
                        ) TO I_ERROS.
        ENDIF.

*       "//Check texto da operação - É obrigatório o preenchimento do texto da atividade da operação ( Ex: operação 0010 - Eliminar vazamento ).
        IF _OPERACAO-DESCRIPTION IS INITIAL.
          APPEND VALUE #(
                          TYPE = 'E'
                          MESSAGE = |O texto da OPeração é Obrigatório!|
                        ) TO I_ERROS.
        ENDIF.

      ENDLOOP.

      IF ME->STATUS_READ( I_EQUNR = |{ _ORDEM-EQUNR }| ) IS NOT INITIAL.
        APPEND VALUE #(
                        TYPE = 'E'
                        MESSAGE = |Equipamento { _ORDEM-EQUNR } está Inativo!|
                      ) TO I_ERROS.
      ENDIF.

      SORT I_ERROS.
      DELETE ADJACENT DUPLICATES FROM I_ERROS COMPARING ALL FIELDS.

      IF I_ERROS IS NOT INITIAL.
        UPDATE ZPMT0016
        SET STATP = 'E'
        WHERE BLOCO EQ _ORDEM-BLOCO
          AND IDORD EQ _ORDEM-IDORD.
        COMMIT WORK.
      ENDIF.

      CALL METHOD ME->LOG_RETORNO
        EXPORTING
          I_FIELD  = 'ORDEM'
          I_BLOCO  = _ORDEM-BLOCO
          I_IDORD  = _ORDEM-IDORD
          I_RETURN = I_ERROS.

    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
