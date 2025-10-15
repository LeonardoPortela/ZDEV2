*&---------------------------------------------------------------------*
*&  Include           ZPMR0078_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_seleciona_dados .

  CONSTANTS: c_stat TYPE jest-stat VALUE 'I0098'.

  IF s_equnr IS NOT INITIAL OR
     s_eqtyp IS NOT INITIAL OR
     s_iwerk IS NOT INITIAL OR
     s_erdat IS NOT INITIAL OR
     s_aedat IS NOT INITIAL.

    "//Get all active vehicles;
    SELECT DISTINCT b~iwerk,a~equnr,a~eqart,a~aedat
      INTO TABLE @DATA(lt_equipment)
      FROM equi AS a
     INNER JOIN equz AS b ON b~equnr = a~equnr
     WHERE a~equnr IN @s_equnr
       AND a~eqtyp IN @s_eqtyp
       AND b~iwerk IN @s_iwerk
       AND a~erdat IN @s_erdat
       AND b~datbi EQ '99991231'.
    IF sy-subrc IS INITIAL.
      IF p_dt_mod IS NOT INITIAL.
        DELETE lt_equipment WHERE aedat <> sy-datum.
      ELSE.
        DELETE lt_equipment WHERE aedat NOT IN s_aedat.
      ENDIF.

    ENDIF.

    DATA(lt_equipament_aux) = lt_equipment.
    SORT lt_equipament_aux ASCENDING BY equnr.
    DELETE ADJACENT DUPLICATES FROM lt_equipament_aux.

    IF lt_equipament_aux IS NOT INITIAL.
      SELECT a~mandt e~tplnr e~pltxt a~equnr a~erdat a~aedat b~eqtyp b~eqart b~objnr a~iwerk a~datbi a~hequi c~eqktx a~rbnr
         FROM equz AS a
         INNER JOIN equi AS b ON b~equnr EQ a~equnr
         INNER JOIN eqkt AS c ON c~equnr EQ b~equnr
         INNER JOIN iloa AS d ON d~iloan EQ a~iloan
         INNER JOIN iflotx AS e ON e~tplnr EQ d~tplnr
         INTO CORRESPONDING FIELDS OF TABLE t_veiculos
        FOR ALL ENTRIES IN lt_equipament_aux
           WHERE a~equnr EQ lt_equipament_aux-equnr
            AND  a~datbi EQ '99991231'
            AND  c~spras EQ sy-langu.


    ENDIF.

  ENDIF.

  IF s_tplnr  IS NOT INITIAL OR
     s_fltyp  IS NOT INITIAL OR
     s_iwerk2 IS NOT INITIAL OR
     s_erdat2 IS NOT INITIAL OR
     s_aedat2 IS NOT INITIAL.

    SELECT *
    FROM iflo AS a
     INTO TABLE t_iflo
    WHERE tplnr IN s_tplnr
      AND swerk IN s_iwerk2
      AND fltyp IN s_fltyp
      AND erdat IN s_erdat2
      AND ( EXISTS ( SELECT * FROM jest
                      WHERE objnr EQ a~objnr
                        AND inact NE abap_true
                        AND stat  EQ c_stat ) ).
    IF sy-subrc IS INITIAL.
      IF p_dtmod2 IS NOT INITIAL.
        DELETE t_iflo WHERE aedat <> sy-datum.
      ELSE.
        DELETE t_iflo WHERE aedat NOT IN s_aedat2.
      ENDIF.
    ENDIF.
  ENDIF.

  IF s_arbpl IS NOT INITIAL OR
     s_werks IS NOT INITIAL OR
     s_begda IS NOT INITIAL OR
     s_aedat3 IS NOT INITIAL.

    SELECT a~arbpl a~werks b~ktext a~begda a~aedat_grnd a~lvorm a~xsprr
       FROM crhd AS a
       INNER JOIN crtx AS b ON b~objid EQ a~objid
       INTO TABLE t_crhd
       WHERE a~arbpl IN s_arbpl
         AND a~werks IN s_werks
         AND a~begda IN s_begda.
    IF sy-subrc IS INITIAL.

      IF p_dtmod3 IS NOT INITIAL.
        DELETE t_crhd WHERE aedat_grnd <> sy-datum.
      ELSE.
        DELETE t_crhd WHERE aedat_grnd NOT IN s_aedat3.
      ENDIF.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_processa_dados .

  DATA: lt_veiculos TYPE TABLE OF ztpm_m_veic_mobile,
        lv_line     TYPE bsvx-sttxt,
        lv_created  TYPE timestampl,
        lv_updated  TYPE timestampl,
        lv_string   TYPE string,
        lt_local    TYPE TABLE OF ztpm_m_local_mobile,
        lt_centro   TYPE TABLE OF ztpm_par_t_ctrab.

  IF t_veiculos IS NOT INITIAL.

    LOOP AT t_veiculos ASSIGNING FIELD-SYMBOL(<fs_veiculos>).

      REFRESH: lt_veiculos.

      IF <fs_veiculos>-objnr IS NOT INITIAL.

        CALL FUNCTION 'STATUS_TEXT_EDIT'
          EXPORTING
            client           = sy-mandt
            objnr            = <fs_veiculos>-objnr
            spras            = sy-langu
          IMPORTING
            line             = lv_line
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc = 0.
          IF lv_line CS 'INAT' OR lv_line CS 'MREL'.
            <fs_veiculos>-istat = '0'.
          ELSE.
            <fs_veiculos>-istat = '1'.
          ENDIF.
        ENDIF.

      ENDIF.
*** Fim - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN

      IF <fs_veiculos>-erdat IS NOT INITIAL.
        CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
          EXPORTING
            i_date = <fs_veiculos>-erdat    " Data
*           i_time = ''  " Hora
          IMPORTING
            e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

        lv_string = lv_created.
        REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
        CONDENSE lv_string NO-GAPS.

        <fs_veiculos>-created_at = lv_string(13).

      ENDIF.

      IF <fs_veiculos>-aedat IS NOT INITIAL.
        CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
          EXPORTING
            i_date = <fs_veiculos>-aedat    " Data
*           i_time = ''  " Hora
          IMPORTING
            e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

        lv_string = lv_updated.
        REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
        CONDENSE lv_string NO-GAPS.

        <fs_veiculos>-updated_at = lv_string(13).

      ENDIF.

      APPEND <fs_veiculos> TO lt_veiculos.

      TRY.

          zcl_envia_equip_mobman=>zif_envia_equip_mobman~get_instance(
                )->set_dados_equip( i_data = lt_veiculos
                )->post_envia_equip( EXPORTING i_equip = lt_veiculos ).

        CATCH zcx_integracao INTO DATA(ex_integra).    "
          ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

        CATCH zcx_error INTO DATA(ex_error).    "  "
          ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

      ENDTRY.

    ENDLOOP.

  ENDIF.

  IF t_iflo IS NOT INITIAL.

    LOOP AT t_iflo ASSIGNING FIELD-SYMBOL(<fs_iflo>).

      REFRESH: lt_local.

      APPEND INITIAL LINE TO lt_local ASSIGNING FIELD-SYMBOL(<fs_local>).

      MOVE-CORRESPONDING <fs_iflo> TO <fs_local>.

      IF <fs_iflo>-objnr IS NOT INITIAL.

        CALL FUNCTION 'STATUS_TEXT_EDIT'
          EXPORTING
            client           = sy-mandt
            objnr            = <fs_iflo>-objnr
            spras            = sy-langu
          IMPORTING
            line             = lv_line
          EXCEPTIONS
            object_not_found = 1
            OTHERS           = 2.
        IF sy-subrc = 0.
          IF lv_line CS 'INAT' OR lv_line CS 'MREL'.
            <fs_local>-istat = '0'.
          ELSE.
            <fs_local>-istat = '1'.
          ENDIF.
        ENDIF.

      ENDIF.


      IF <fs_iflo>-erdat IS NOT INITIAL.

        CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
          EXPORTING
            i_date = <fs_iflo>-erdat     " Data
          IMPORTING
            e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

        lv_string = lv_created.
        REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
        CONDENSE lv_string NO-GAPS.

        <fs_local>-created_at = lv_string(13).

      ENDIF.

      IF <fs_iflo>-aedat IS NOT INITIAL.

        CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
          EXPORTING
            i_date = <fs_iflo>-aedat    " Data
*           i_time = hora  " Hora
          IMPORTING
            e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

        lv_string = lv_updated.
        REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
        CONDENSE lv_string NO-GAPS.

        <fs_local>-updated_at = lv_string(13).

      ENDIF.

      TRY.
          zcl_envia_local_instal_mobman=>zif_envia_local_instal_mobman~get_instance(
                )->set_dados_local_instal( i_data = lt_local
                )->post_envia_local_instal( EXPORTING i_local = lt_local ).

        CATCH zcx_integracao INTO ex_integra.    "
          ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

        CATCH zcx_error INTO ex_error.    "  "
          ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

      ENDTRY.

    ENDLOOP.



  ENDIF.


  IF t_crhd IS NOT INITIAL.
    LOOP AT t_crhd ASSIGNING FIELD-SYMBOL(<fs_crhd>).

      REFRESH: lt_centro.

      APPEND INITIAL LINE TO lt_centro ASSIGNING FIELD-SYMBOL(<fs_centro>).

      MOVE-CORRESPONDING <fs_crhd> TO <fs_centro>.

      IF <fs_crhd>-lvorm IS NOT INITIAL OR <fs_crhd>-xsprr IS NOT INITIAL.
        <fs_centro>-istat = '1'.
      ELSE.
        <fs_centro>-istat = '0'.
      ENDIF.
*** Fim - Rubenilson Pereira - 19.07.2022 - US83281 - MOBMAN

      IF <fs_crhd>-begda IS NOT INITIAL.
        CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
          EXPORTING
            i_date = <fs_crhd>-begda    " Data
*           i_time = ''  " Hora
          IMPORTING
            e_date = lv_created.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

        lv_string = lv_created.
        REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
        CONDENSE lv_string NO-GAPS.

        <fs_centro>-created_at = lv_string(13).
      ENDIF.


      IF <fs_crhd>-aedat_grnd IS NOT INITIAL.
        CALL FUNCTION 'Z_CONV_DATE_TO_TIMESTAMP_MILIS'
          EXPORTING
            i_date = <fs_crhd>-aedat_grnd   " Data
*           i_time = ''  " Hora
          IMPORTING
            e_date = lv_updated.    " Registro hora UTC forma descritiva (JJJJMMTThhmmssmmmuuun)

        lv_string = lv_updated.
        REPLACE ALL OCCURRENCES OF '.' IN lv_string WITH space.
        CONDENSE lv_string NO-GAPS.

        <fs_centro>-updated_at = lv_string(13).
      ENDIF.

      TRY.
          zcl_envia_centro_mobman=>zif_envia_centro_mobman~get_instance(
                )->set_dados_centro( i_data = lt_centro
                )->post_envia_centro( EXPORTING i_centro = lt_centro ).

        CATCH zcx_integracao INTO ex_integra.    "
          ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

        CATCH zcx_error INTO ex_error.    "  "
          ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).

      ENDTRY.

    ENDLOOP.

  ENDIF.
ENDFORM.
