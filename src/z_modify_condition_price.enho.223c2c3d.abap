"Name: \PR:SAPLV61A\FO:USEREXIT_XKOMV_BEWERTEN_END\SE:END\EI
ENHANCEMENT 0 Z_MODIFY_CONDITION_PRICE.

  DATA: v_field_vfsi TYPE c LENGTH 30.

  FIELD-SYMBOLS: <field_vfsi> TYPE ANY TABLE.

  DATA: t_vfsi TYPE TABLE OF vfsivb WITH HEADER LINE,
        t_vttp TYPE TABLE OF vttp.

*-#133089-21.02.2024-JT-inicio
*  DATA: t_callstack  TYPE abap_callstack,
*        lc_fat_autom TYPE char01.

  CALL FUNCTION 'SYSTEM_CALLSTACK'
    EXPORTING
      max_level = 0
    IMPORTING
      callstack = t_callstack.

*------------------
* faturamento automatico
*------------------
  READ TABLE t_callstack INTO w_callstack WITH KEY mainprogram = 'ZLESR0180_JOB'.
  IF sy-subrc = 0.
    lc_fat_autom = abap_true.
  ENDIF.
*-#133089-21.02.2024-JT-fim

  IF  ( sy-tcode EQ 'VI01'      ) OR
      ( sy-tcode EQ 'VI02'      ) OR
      ( sy-tcode EQ 'VT01N'     ) OR
      ( sy-tcode EQ 'VT01'      ) OR
      ( sy-tcode EQ 'VT02N'     ) OR
      ( sy-tcode EQ 'VT02'      ) OR
      ( sy-tcode EQ 'ZLES0106'  ) OR
      ( sy-tcode EQ 'ZLES0113'  ) OR
      ( sy-tcode EQ 'ZMM0079'   ) OR
      ( sy-tcode EQ 'ZMM0127'   ) OR
      ( sy-tcode EQ 'ZLES0136'  ) OR
      ( sy-tcode EQ 'ZLES0115'  ) OR
      ( lc_fat_autom = abap_true ).

    CLEAR: t_vttp[].

    v_field_vfsi = '(SAPLV54B)G_VFSI[]'.
    ASSIGN (v_field_vfsi) TO <field_vfsi>.

    "Controle de remessa
    IF <field_vfsi> IS ASSIGNED.

      t_vfsi[] = <field_vfsi>[].

      IF t_vfsi[] IS NOT INITIAL.
        SELECT *
         FROM vttp INTO TABLE t_vttp
          FOR ALL ENTRIES IN t_vfsi
        WHERE vbeln = t_vfsi-vbeln.
      ENDIF.

    ENDIF.

    IF t_vttp[] IS NOT INITIAL.

      CALL FUNCTION 'Z_MODIFY_CONDITION_PRICE_01'
        TABLES
          t_vttp   = t_vttp
          c_komv   = xkomv
          t_vfsivb = t_vfsi.
    ENDIF.

  ENDIF.

ENDENHANCEMENT.
