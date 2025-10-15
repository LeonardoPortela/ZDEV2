*&---------------------------------------------------------------------*
*&  Include  ZPM_AUTOMATIZA_CAMPOS_IE01
*&---------------------------------------------------------------------*

IF sy-tcode EQ 'IE01' OR sy-tcode EQ 'IE02'.

  FIELD-SYMBOLS: <fs_itob>     TYPE itob,
                 <fs_itobattr> TYPE itobattr,
                 <fs_fleet>    TYPE fleet.

  DATA: lr_equnr TYPE RANGE OF equi-equnr,
        lv_seq   TYPE numc4,
        lv_qtd   TYPE sy-tabix.

  ASSIGN ('(SAPLITO0)ITOB')     TO <fs_itob>.
  ASSIGN ('(SAPLITO0)ITOBATTR') TO <fs_itobattr>.
  ASSIGN ('(SAPLITO0)FLEET')    TO <fs_fleet>.

  IF <fs_itob> IS ASSIGNED.

    "Inicio Marcio Miguel 17.02.2023  102916 Atribuição Perfil de Catálogo na ZPM0017
    CASE  <fs_itob>-eqtyp.
      WHEN '1' OR '2' OR '3' OR '4'.

        <fs_itob>-ingrp = 'FRO'.
        <fs_itob>-tidnr = <fs_itob>-equnr.
        <fs_itob>-eqfnr = 'AMBIENTALMENTE CRÍTICO'.

        IF <fs_itob>-swerk IS NOT INITIAL .
          <fs_itob>-gsber = <fs_itob>-swerk.
          <fs_itob>-iwerk = <fs_itob>-swerk.
        ENDIF.


        IF <fs_itob>-rbnr IS INITIAL.
          SELECT SINGLE rbnr
            FROM zpmr0001
            INTO <fs_itob>-rbnr
            WHERE herst = <fs_itob>-herst AND
                  typbz = <fs_itob>-typbz AND
                  class_oper = <fs_fleet>-fleet_cat.
        ENDIF.

        IF <fs_fleet> IS ASSIGNED.
          <fs_fleet>-fleet_num = <fs_fleet>-zzimobilizado.

          IF <fs_fleet>-license_num IS INITIAL.
            <fs_fleet>-license_num = 'N/A'.
          ENDIF.

          IF <fs_fleet>-card_num IS INITIAL.
            <fs_fleet>-card_num = 'N/A'.
          ENDIF.

          IF <fs_itob> IS ASSIGNED.
            <fs_fleet>-chassis_num = <fs_itob>-serge.
          ENDIF.

        ENDIF.
      WHEN OTHERS.

    ENDCASE.

    "Final Marcio Miguel 17.02.2023  102916 Atribuição Perfil de Catálogo na ZPM0017
  ENDIF.

  IF <fs_itobattr> IS ASSIGNED.


    IF <fs_itob> IS ASSIGNED.
      IF  'IE01_IE31' CS sy-tcode.
        IF <fs_itob>-eqtyp EQ '1' OR <fs_itob>-eqtyp EQ '2' OR <fs_itob>-eqtyp EQ '3' OR <fs_itob>-eqtyp EQ '4'.

          <fs_itobattr>-gewrk = 'OFICINA'.

          IF <fs_itob>-eqart IS NOT INITIAL.

            APPEND INITIAL LINE TO lr_equnr ASSIGNING FIELD-SYMBOL(<fs_equnr>).
            <fs_equnr>-sign = 'I'.
            <fs_equnr>-option = 'CP'.
            CONCATENATE <fs_itob>-eqtyp <fs_itob>-eqart '****' INTO <fs_equnr>-low.

            SHIFT <fs_equnr>-low RIGHT DELETING TRAILING ' '.
            TRANSLATE <fs_equnr>-low USING ' 0'.


            SELECT MAX( equnr )
              FROM equi
              INTO @DATA(lv_equnr)
              WHERE equnr IN @lr_equnr.
            IF sy-subrc IS INITIAL AND lv_equnr IS NOT INITIAL.

              lv_qtd = strlen( lv_equnr ).
              lv_qtd = lv_qtd - 4.
              lv_seq = lv_equnr+lv_qtd.

            ENDIF.

            lv_seq = lv_seq + 1.

            CONCATENATE <fs_itob>-eqtyp <fs_itob>-eqart lv_seq INTO <fs_itobattr>-equnr.
            SHIFT <fs_itobattr>-equnr RIGHT DELETING TRAILING ' '.
            TRANSLATE <fs_itobattr>-equnr USING ' 0'.

*** Stefanini - IR195205 - 18/09/2024 - LAZAROSR - Início de Alteração
            DATA: lo_manutentor_equnr TYPE REF TO zcl_manutentor_equnr.

            lo_manutentor_equnr = NEW #( i_v_equnr      = <fs_itobattr>-equnr
                                         i_v_klassenart = <fs_itobattr>-klassenart ).

            lo_manutentor_equnr->iniciar( ).
*** Stefanini - IR195205 - 18/09/2024 - LAZAROSR - Fim de Alteração


          ENDIF.
        ENDIF.
      ENDIF.
      <fs_itobattr>-wergw = <fs_itob>-swerk.
    ENDIF.

  ENDIF.
ENDIF.
