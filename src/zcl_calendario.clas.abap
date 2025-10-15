class ZCL_CALENDARIO definition
  public
  final
  create public .

public section.

  class-methods GET_CALENDARIO
    importing
      value(I_BUKRS) type BUKRS
      value(I_TIPO_PROCESSO) type ZE_TP_PROC optional
      value(I_NOME_PROCESSO) type ZE_NOMEP optional
    exporting
      !E_HOLIDAY_CALENDAR type SCAL-HCALID
      !E_FACTORY_CALENDAR type SCAL-FCALID .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CALENDARIO IMPLEMENTATION.


  METHOD get_calendario.

    SELECT SINGLE * FROM zfit0215
      INTO @DATA(wa_zfit0215)
      WHERE bukrs    EQ @i_bukrs.
*        AND tp_proc  EQ @i_tipo_processo
*        AND nome_p   EQ @i_nome_processo.
    IF sy-subrc IS INITIAL.
      e_holiday_calendar = wa_zfit0215-hcalid.
      e_factory_calendar = wa_zfit0215-fcalid.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
