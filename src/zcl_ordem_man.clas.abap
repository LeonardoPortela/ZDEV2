class ZCL_ORDEM_MAN definition
  public
  final
  create public .

public section.

  class-methods M_CHECK_ORC_INIC_ORD
    importing
      !I_AUFNR type AUFNR
    exporting
      !I_RETORN type CHAR01
      !I_BELNR type BPEG-BELNR .
  class-methods M_CHECK_ORC_SUPL_ORD
    importing
      !I_AUFNR type AUFNR
    exporting
      !I_RETORN type CHAR01
      !I_BELNR type BPEG-BELNR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ORDEM_MAN IMPLEMENTATION.


  METHOD M_CHECK_ORC_INIC_ORD.
    "Verificar se foi registrado orçamento para ordem.
    SELECT SINGLE *
    FROM AUFK
    INTO @DATA(_AUFK)
      WHERE AUFNR EQ @I_AUFNR.

    IF _AUFK IS NOT INITIAL.
      SELECT SINGLE * "#EC CI_DB_OPERATION_OK[2226072]
      FROM BPEG       "#EC CI_DB_OPERATION_OK[2226048]
      INTO @DATA(_BPEG)
        WHERE OBJNR EQ @_AUFK-OBJNR
          AND VORGA EQ 'KBUD'
          AND PLDAT NE '00000000'.

      IF _BPEG IS NOT INITIAL.
        IF _BPEG-WTGES EQ _AUFK-USER4.
          I_RETORN = ABAP_TRUE.
          I_BELNR  = _BPEG-BELNR.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD M_CHECK_ORC_SUPL_ORD.
    DATA: R_BELNR TYPE ZRSDSSELOPTS.
    SELECT * FROM ZPMR0006 INTO TABLE @DATA(T_ZPMR0006) WHERE AUFNR EQ @I_AUFNR.
    R_BELNR = VALUE #( FOR LS IN T_ZPMR0006 ( SIGN = 'I' OPTION = 'EQ' LOW = LS-BELNR HIGH = '' ) ) .

    IF T_ZPMR0006 IS NOT INITIAL.
      SELECT SINGLE * FROM AUFK INTO @DATA(_AUFK) WHERE AUFNR EQ @I_AUFNR.
      SELECT SINGLE * FROM BPEG INTO @DATA(_BPEG)
        WHERE OBJNR EQ @_AUFK-OBJNR
        AND VORGA EQ 'KBN0'
        AND PLDAT NE '00000000'
        AND BELNR NOT IN @R_BELNR.

      IF _BPEG IS NOT INITIAL.

        READ TABLE T_ZPMR0006 ASSIGNING FIELD-SYMBOL(<W_ZPMR0006>) WITH KEY AUFNR = I_AUFNR.
* ---> S4 Migration - 10/06/2023 - DG
        "IF _BPEG-WTGES EQ <W_ZPMR0006>-VLR_ESTIMADO.
        data: lv_WTGES type dmbtr.

        lv_WTGES = conv #( _BPEG-WTGES ).

        IF lv_WTGES EQ <W_ZPMR0006>-VLR_ESTIMADO.
* <--- S4 Migration - 10/06/2023 - DG


          I_RETORN = ABAP_TRUE.
          I_BELNR  = _BPEG-BELNR.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
