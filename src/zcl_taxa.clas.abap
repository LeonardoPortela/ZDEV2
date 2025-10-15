class ZCL_TAXA definition
  public
  final
  create public .

public section.

  types:
    MTY_TAXRATE(12) TYPE p DECIMALS 6 .

  class-methods GET_CK_PIS_COFINS
    importing
      !I_MWSKZ type MWSKZ
    changing
      !E_CHECK type CHAR01 .
  class-methods GET_PIS_COFINS
    importing
      !I_MWSKZ type MWSKZ
    changing
      !E_PIS_RATE type MTY_TAXRATE
      !E_COFINS_RATE type MTY_TAXRATE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_TAXA IMPLEMENTATION.


  METHOD GET_CK_PIS_COFINS.

    E_CHECK = ABAP_FALSE.

    SELECT SINGLE * INTO @DATA(WA_ZMMT0125)
      FROM ZMMT0125
     WHERE MWSKZ EQ @I_MWSKZ
       AND MWSKZ NE @SPACE.

    IF SY-SUBRC IS INITIAL.
      E_CHECK = ABAP_TRUE.
    ENDIF.

  ENDMETHOD.


  METHOD GET_PIS_COFINS.

    SELECT SINGLE * INTO @DATA(WA_ZMMT0125)
      FROM ZMMT0125
     WHERE MWSKZ EQ @I_MWSKZ
       AND MWSKZ NE @SPACE.

    IF SY-SUBRC IS INITIAL.
      E_PIS_RATE = WA_ZMMT0125-NM_ALI_PIS / 100.
      E_COFINS_RATE = WA_ZMMT0125-NM_ALI_COFINS / 100.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
