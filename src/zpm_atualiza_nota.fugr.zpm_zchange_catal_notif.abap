FUNCTION zpm_zchange_catal_notif .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_QMNUM) TYPE  QMNUM
*"  EXPORTING
*"     REFERENCE(R_RESULT) TYPE  BAPIRET2_T
*"  TABLES
*"      T_DADOS_CATALO STRUCTURE  ZPME0072
*"      T_TEXTO_LOGO STRUCTURE  BAPI2080_NOTFULLTXTI OPTIONAL
*"----------------------------------------------------------------------


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

    txtne   TYPE TABLE OF bapi2080_notfulltxte.
*    txtni   TYPE TABLE OF bapi2080_notfulltxti.


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

  SELECT SINGLE * FROM viqmel INTO @DATA(ws_viqmel) WHERE qmnum EQ @i_qmnum.

  LOOP AT t_dados_catalo INTO DATA(_item).

    DATA(_item_key) = sy-tabix.

    IF ws_viqmel-qmart EQ 'Y3'.

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

  IF i_qmnum IS NOT INITIAL.
*
*    CALL FUNCTION 'BAPI_ALM_NOTIF_GET_DETAIL'
*      EXPORTING
*        number             = i_qmnum
*      IMPORTING
*        notifheader_export = _notifheader_export
*      TABLES
*        notlongtxt         = txtne
*        notitem            = gitem
*        notifcaus          = gcaus
*        notifactv          = gfact
*        return             = t_return.
*
*    CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_MODIFY'
*      EXPORTING
*        number             = i_qmnum
*      IMPORTING
*        notifheader_export = _notifheader_export
*      TABLES
*        notifitem          = lt_item_mod
*        notifitem_x        = lt_item_modx
*        notifcaus          = lt_caus_mod
*        notifcaus_x        = lt_caus_modx
*        notifactv          = lt_fact_mod
*        notifactv_x        = lt_fact_mod
*        return             = t_return.

*    IF t_return IS NOT INITIAL AND NOT line_exists( t_return[ type = 'E' ] ).
*
*      CALL FUNCTION 'IQS4_SAVE_NOTIFICATION'
*        EXPORTING
*          i_qmnum            = _notifheader_export-notif_no
*          i_commit           = abap_true
*          i_wait             = abap_true
*          i_refresh_complete = abap_true
*        TABLES
*          return             = t_return.
*
*    ENDIF.

*    ditem = VALUE #( FOR ls4 IN t_notitem_del   ( CORRESPONDING #( ls4 ) ) ).
*    dcaus = VALUE #( FOR ls5 IN t_notifcaus_del ( CORRESPONDING #( ls5 ) ) ).
*    dfact = VALUE #( FOR ls6 IN t_notifactv_del ( CORRESPONDING #( ls6 ) ) ).
*
*    IF ditem IS NOT INITIAL OR dcaus IS NOT INITIAL OR dfact IS NOT INITIAL.
*
*      FREE: t_return.
*      CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_DELETE'
*        EXPORTING
*          number    = i_qmnum
*        TABLES
*          notitem   = ditem
*          notifcaus = dcaus
*          notifactv = dfact
*          return    = t_return.
*
*      APPEND LINES OF t_return TO r_result.
*
*      IF NOT line_exists( t_return[ type = 'E' ] ).
*
*        FREE: t_return.
*        CALL FUNCTION 'IQS4_SAVE_NOTIFICATION'
*          EXPORTING
*            i_qmnum            = _notifheader_export-notif_no
*            i_commit           = abap_true
*            i_wait             = abap_true
*            i_refresh_complete = abap_true
*          TABLES
*            return             = t_return.
*
*        APPEND LINES OF t_return TO r_result.
*      ENDIF.

*    ENDIF.

*      txtni = VALUE #(
*      FOR ls IN t_txt (
*                        objtype    = 'QMEL'
*                        objkey     = '00000000'
*                        format_col = 'X'
*                        text_line  = ls-txt
*                       )
*                      ).

    aitem = VALUE #( FOR ls1 IN t_notitem  ( CORRESPONDING #( ls1 ) ) ).
    acaus = VALUE #( FOR ls2 IN t_notifcaus ( CORRESPONDING #( ls2 ) ) ).
    afact = VALUE #( FOR ls3 IN t_notifactv ( CORRESPONDING #( ls3 ) ) ).

    TRY .

*        FREE: afact.
*        IF gfact IS NOT INITIAL.
*          afact[] = gfact[].
*        ENDIF.

        IF aitem IS NOT INITIAL OR acaus IS NOT INITIAL OR afact IS NOT INITIAL OR t_texto_logo IS NOT INITIAL.

          FREE: t_return.
          CALL FUNCTION 'BAPI_ALM_NOTIF_DATA_ADD'
            EXPORTING
              number             = i_qmnum
*             notifheader        = _notifheader
            IMPORTING
              notifheader_export = _notifheader_export
            TABLES
              notfulltxt         = t_texto_logo
              notitem            = aitem
              notifcaus          = acaus
              notifactv          = gfact
              return             = t_return.


          IF NOT line_exists( t_return[ type = 'E' ] ).

            FREE: t_return.
            CALL FUNCTION 'IQS4_SAVE_NOTIFICATION'
              EXPORTING
                i_qmnum            = _notifheader_export-notif_no
                i_commit           = abap_true
                i_wait             = abap_true
                i_refresh_complete = abap_true
              TABLES
                return             = t_return.

            APPEND LINES OF t_return TO r_result.

          ELSE.

            APPEND LINES OF t_return TO r_result.
          ENDIF.
        ENDIF.


      CATCH cx_root INTO DATA(excp).
        DATA(etext) = excp->if_message~get_text( ).
        APPEND VALUE #( type = 'E' message = etext ) TO _return.
    ENDTRY.
  ENDIF.
ENDFUNCTION.
