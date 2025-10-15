*&---------------------------------------------------------------------*
*&  Include           ZPM_DISPARA_API_NOTA_MOB
*&---------------------------------------------------------------------*

TYPES: tb_wqmfe TYPE SORTED TABLE OF wqmfe   WITH UNIQUE KEY qmnum fenum,
       tb_wqmma TYPE SORTED TABLE OF wqmma   WITH UNIQUE KEY qmnum manum,
       tb_wqmur TYPE SORTED TABLE OF wqmur   WITH UNIQUE KEY qmnum fenum urnum,
*       tb_viqmma TYPE TABLE OF rfc_viqmma, " RJF Rollback QAS - Bug Solto 146832
       tb_qmel  TYPE SORTED TABLE OF iqs0_waqm WITH UNIQUE KEY qmnum.

" RJF Rollback QAS - Bug Solto 146832
**"FF - 24/04/2024 - inicio #131818
**TYPES:  BEGIN OF ty_zaponta.
**          INCLUDE TYPE zoperations.
**TYPES:    aufnr      TYPE aufk-aufnr,
**          ktext      TYPE crtx-ktext,
**          werks      TYPE aufk-werks,
**          budat      TYPE afru-budat,
**          ltxa1      TYPE afvc-ltxa1,
**          ktext1     TYPE aufk-ktext,
**          begzt      TYPE sy-uzeit,
**          endzt      TYPE sy-uzeit,
**          pause      TYPE sy-uzeit,
**          ngrad      TYPE kako-ngrad,
**          ueberlast  TYPE kako-ueberlast,
**          einzh      TYPE p DECIMALS 2,
**          einzt      TYPE p DECIMALS 2,
**          v_sobrcarg TYPE p DECIMALS 2,
**          istru      TYPE afvc-istru,
**          marc       TYPE c,
**        END OF ty_zaponta.
**
**DATA: lt_atividades    TYPE zpm_t_atividades,
**      it_aponta_export TYPE TABLE OF ty_zaponta.
**
**"FF - 24/04/2024 - fim #131818

FIELD-SYMBOLS: <fs_qmfe> TYPE tb_wqmfe,
               <fs_qmma> TYPE tb_wqmma,
               <fs_qmur> TYPE tb_wqmur,
               <fs_qmel> TYPE tb_qmel.

system_status_line_exp = system_status_line.
user_status_line_exp   = user_status_line .

IF sy-tcode NE 'IW31' AND sy-tcode NE 'IW32'.

  IF ( sy-ucomm EQ 'WTER' OR sy-ucomm EQ 'BUCH' OR sy-ucomm EQ 'WIAR' ).

    DATA: lw_nota    TYPE zepm_d_nota,
          lt_objnr   TYPE ztpm_d_n_catalag_t,
          t_texto    TYPE TABLE OF tline,
          w_texto    TYPE tline,
          l_name     TYPE thead-tdname,
          lv_created TYPE timestampl,
          lv_updated TYPE timestampl,
          lv_string  TYPE string,
          lv_ausvn   TYPE viqmel-ausvn,
          lv_auztv   TYPE viqmel-auztv,
          lv_msg     TYPE string.


    ASSIGN ('(SAPLIQS1)B_WQMFE[]')   TO <fs_qmfe>.
    ASSIGN ('(SAPLIQS1)B_WQMMA[]')   TO <fs_qmma>.
    ASSIGN ('(SAPLIQS1)B_WQMUR[]')   TO <fs_qmur>.
    ASSIGN ('(SAPLIQS1)B_WVIQMEL[]') TO <fs_qmel>.

    IF <fs_qmel> IS ASSIGNED.

      READ TABLE <fs_qmel> ASSIGNING FIELD-SYMBOL(<fs_qmel_line>) INDEX 1.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING <fs_qmel_line> TO lw_nota.

        IF lw_nota-ausvn EQ lv_ausvn.
          lw_nota-ausvn = sy-datum.
        ENDIF.

        IF lw_nota-auztv EQ lv_auztv.
          lw_nota-auztv = sy-uzeit.
        ENDIF.

        IF <fs_qmel_line>-arbpl IS NOT INITIAL.

          SELECT SINGLE arbpl
            FROM crhd
            INTO lw_nota-arbpl
            WHERE objid = <fs_qmel_line>-arbpl.

        ENDIF.

        IF system_status_line_exp CS 'MSPR'.
          lw_nota-istat = '1'.
        ELSEIF system_status_line_exp CS 'MSEN'.
          lw_nota-istat = '2'.
        ELSEIF system_status_line_exp CS 'MSPN'.
          lw_nota-istat = '0'.
        ENDIF.

        l_name = <fs_qmel_line>-qmnum.

        CALL FUNCTION 'READ_TEXT'
          EXPORTING
            id                      = 'LTXT'
            language                = sy-langu
            name                    = l_name
            object                  = 'QMEL'
          TABLES
            lines                   = t_texto
          EXCEPTIONS
            id                      = 1
            language                = 2
            name                    = 3
            not_found               = 4
            object                  = 5
            reference_check         = 6
            wrong_access_to_archive = 7
            OTHERS                  = 8.

        LOOP AT t_texto ASSIGNING FIELD-SYMBOL(<_text_l>).
          IF lw_nota-txtnt IS INITIAL.
            lw_nota-txtnt =  | ->{ <_text_l>-tdline }| .
          ELSE.
            lw_nota-txtnt = |{ lw_nota-txtnt } ->{ <_text_l>-tdline }|.
          ENDIF.
        ENDLOOP.


        IF <fs_qmel_line>-erdat IS NOT INITIAL.
          CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
            EXPORTING
              i_date = <fs_qmel_line>-erdat   " Data
              i_time = <fs_qmel_line>-erzeit
            IMPORTING
              e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

          lv_string = lv_created.
          REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
          CONDENSE lv_string NO-GAPS.

          lw_nota-created_at = lv_string(13).

        ENDIF.

        IF <fs_qmel_line>-aedat IS NOT INITIAL.
          CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
            EXPORTING
              i_date = <fs_qmel_line>-aedat   " Data
              i_time = <fs_qmel_line>-aezeit
            IMPORTING
              e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

          lv_string = lv_updated.
          REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
          CONDENSE lv_string NO-GAPS.

          lw_nota-updated_at = lv_string(13).

        ENDIF.

      ENDIF.


      SELECT *
        FROM tqscr
        INTO TABLE @DATA(t_tqscr)
        FOR ALL ENTRIES IN @<fs_qmel>
        WHERE qmart EQ @<fs_qmel>-qmart
          AND qmtyp EQ '01'
          AND tabcd EQ '10\TAB00'.
      SORT t_tqscr[] BY sub03 ASCENDING.
      DELETE t_tqscr[] WHERE sub03 NE '035'.
      SORT t_tqscr[] BY qmart qmtyp tabcd ASCENDING.

      LOOP AT <fs_qmfe> ASSIGNING FIELD-SYMBOL(<fs_qmfe_line>).
        APPEND INITIAL LINE TO lt_objnr ASSIGNING FIELD-SYMBOL(<fs_objnr>).

        READ TABLE t_tqscr ASSIGNING FIELD-SYMBOL(<_tqscr>)
        WITH KEY qmart = lw_nota-qmart
                 qmtyp = '01'
                 tabcd = '10\TAB00'
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.

          <fs_objnr>-numero_item = <fs_qmfe_line>-fenum.
          <fs_objnr>-otkat = <fs_qmfe_line>-otkat.
          <fs_objnr>-otgrp = <fs_qmfe_line>-otgrp.
          <fs_objnr>-oteil = <fs_qmfe_line>-oteil.
          <fs_objnr>-fekat = <fs_qmfe_line>-fekat.
          <fs_objnr>-fegrp = <fs_qmfe_line>-fegrp.
          <fs_objnr>-fecod = <fs_qmfe_line>-fecod.
          <fs_objnr>-fetxt = <fs_qmfe_line>-fetxt.

          IF <fs_qmfe_line>-aeknz EQ 'D' .

            <fs_objnr>-eliminado = '1'.

          ELSE.

            <fs_objnr>-eliminado = '0'.

          ENDIF.

          IF <fs_qmur> IS ASSIGNED.
            READ TABLE <fs_qmur> ASSIGNING FIELD-SYMBOL(<fs_qmur_line>)
            WITH KEY qmnum = <fs_qmfe_line>-qmnum
                     fenum = <fs_qmfe_line>-fenum
            BINARY SEARCH.
            IF sy-subrc IS INITIAL.
              <fs_objnr>-urkat = <fs_qmur_line>-urkat.
              <fs_objnr>-urgrp = <fs_qmur_line>-urgrp.
              <fs_objnr>-urcod = <fs_qmur_line>-urcod.
              <fs_objnr>-urstx = <fs_qmur_line>-urtxt.
            ENDIF.
          ENDIF.
        ELSE.

          LOOP AT <fs_qmma> ASSIGNING FIELD-SYMBOL(<fs_qmma_line>).

            <fs_objnr>-numero_item = <fs_qmma_line>-manum.
            <fs_objnr>-mnkat =  <fs_qmma_line>-mnkat.
            <fs_objnr>-mngrp =  <fs_qmma_line>-mngrp.
            <fs_objnr>-mncod =  <fs_qmma_line>-mncod.

            IF <fs_qmma_line>-aeknz EQ 'D'.
              <fs_objnr>-eliminado = '1'.
            ELSE.
              <fs_objnr>-eliminado = '0'.
            ENDIF.

          ENDLOOP.

        ENDIF.

      ENDLOOP.

      lw_nota-part_objnr = lt_objnr.

**-US 146832-02-08-2024-#146832-RJF-início
** Bug Solto 146832 - Não apagar trechos comentados
*
*      TRY.
*
*          zcl_cria_modifica_nota_mobman=>zif_cria_modifica_nota_mobman~get_instance(
*                )->set_dados_nota( i_data = lw_nota
*                )->post_cria_modifica_nota_mobman( EXPORTING i_nota = lw_nota ).
*
*        CATCH zcx_integracao INTO DATA(ex_integra).    "
*          ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
*
*        CATCH zcx_error INTO DATA(ex_error).    "  "
*          ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
*
*      ENDTRY.
*
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 INTO lv_msg.
*      CONCATENATE lv_msg 'Nota' lw_nota-qmnum INTO lv_msg SEPARATED BY space.
*
*      MESSAGE lv_msg TYPE 'S'.
*
** Bug Solto 146832 - Não apagar trechos comentados
**-US 146832-02-08-2024-#146832-RJF-fim


    ENDIF.
  ENDIF.

*" RJF Rollback QAS - Bug Solto 146832 Ini
*  "FF - 23/04/2024 - inicio #131818
*  IF sy-tcode = 'ZPM0102' AND sy-ucomm EQ 'SAVE'. "Envio de dados das Atividades/Ações para o Mobman
*" RJF Rollback QAS - Bug Solto 146832 fim

*    FIELD-SYMBOLS: <fs_viqmma> TYPE tb_viqmma.
*
*    ASSIGN ('(SAPLIWON)T_VIQMMA[]') TO <fs_viqmma>.
*    ASSIGN ('(ZPMR0082)V_NOTA') TO FIELD-SYMBOL(<lv_qmnum>).
*
*
*    IF <fs_viqmma> IS NOT INITIAL.
*
*      IMPORT it_aponta_export TO it_aponta_export[] FROM MEMORY ID 'ZPM0102'. "Export feito no include ZPMR0028_METHOD
*
*      LOOP AT <fs_viqmma> ASSIGNING FIELD-SYMBOL(<wa_qmma>).
*
*        READ TABLE it_aponta_export INDEX sy-tabix INTO DATA(wa_aponta).
*        IF sy-subrc <> 0.
*          CLEAR wa_aponta.
*        ENDIF.
*
*        APPEND INITIAL LINE TO lt_atividades ASSIGNING FIELD-SYMBOL(<fs_ativ>).
*
*        <fs_ativ>-id     = <lv_qmnum>.
*        <fs_ativ>-qmnum  = <lv_qmnum>.
*        <fs_ativ>-n_item = <wa_qmma>-manum.
*        <fs_ativ>-vornr  = wa_aponta-activity.
*        <fs_ativ>-pernr  = wa_aponta-pernr.
*        <fs_ativ>-mnkat  = <wa_qmma>-mnkat.
*        <fs_ativ>-mngrp  = <wa_qmma>-mngrp.
*        <fs_ativ>-mncod  = <wa_qmma>-mncod.
*        <fs_ativ>-isdd   = wa_aponta-isdd.
*        <fs_ativ>-isdz   = wa_aponta-isdz.
*        <fs_ativ>-iedd   = wa_aponta-iedd.
*        <fs_ativ>-iedz   = wa_aponta-iedz.
*
**      ENDLOOP.
*
*        MOVE <lv_qmnum> TO lw_nota-qmnum.
*
*        TRY.
*
*            zcl_cria_modifica_nota_mobman=>zif_cria_modifica_nota_mobman~get_instance(
*                  )->set_dados_nota( i_data = lw_nota "Passando vazio, pois está sendo enviado as ações/atividades
*                                     i_atividades = lt_atividades
*                  )->post_cria_modifica_nota_mobman( EXPORTING i_nota = lw_nota
*                                                               i_atividades = lt_atividades ).
*
*          CATCH zcx_integracao INTO ex_integra.    "
*            ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
*
*          CATCH zcx_error INTO ex_error.    "  "
*            ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
*
*        ENDTRY.
*
*        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 INTO lv_msg.
*        CONCATENATE lv_msg 'Nota' <lv_qmnum> INTO lv_msg SEPARATED BY space.
*
*        MESSAGE lv_msg TYPE 'S'.
*
*      ENDLOOP.
*
*    ENDIF.
*  ENDIF.
  " RJF Rollback QAS - Bug Solto 146832 fim
ENDIF.
"FF - 23/04/2024 - fim #131818
