class ZCL_MANUTENTOR_EQUNR definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !I_V_EQUNR type EQUNR
      !I_V_KLASSENART type KLASSENART .
  methods INICIAR .
protected section.
private section.

  data V_EQUNR type EQUNR .
  data V_KLASSENART type KLASSENART .
  constants C_TABL type INOB-OBTAB value 'EQUI' ##NO_TEXT.
  constants C_DELETE type CHAR1 value 'X' ##NO_TEXT.
  constants C_KLART type KLASSENART value '002' ##NO_TEXT.

  methods VERIFICAR_BLOQUEIO_EQUNR
    returning
      value(R_V_BLOQUEADO) type BOOLEAN .
  methods REALIZAR_DESBLOQUEIO_EQUNR .
  methods EXIBIR_MSG_PROCESSAMENTO
    importing
      !I_V_MSG type STRING .
ENDCLASS.



CLASS ZCL_MANUTENTOR_EQUNR IMPLEMENTATION.


  METHOD constructor.

    me->v_equnr      = i_v_equnr.
    me->v_klassenart = i_v_klassenart.

  ENDMETHOD.


  METHOD exibir_msg_processamento.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        text = i_v_msg.

  ENDMETHOD.


  METHOD iniciar.

    TRY.

        IF me->verificar_bloqueio_equnr( ).

          me->realizar_desbloqueio_equnr( ).

        ENDIF.

      CATCH cx_root.

    ENDTRY.

  ENDMETHOD.


  METHOD realizar_desbloqueio_equnr.

    IF me->v_klassenart IS NOT INITIAL.

      SUBMIT rcclzuob WITH klart  = me->v_klassenart
                      WITH tabl   = me->c_tabl
                      WITH delete = me->c_delete
                      EXPORTING LIST TO MEMORY AND RETURN.

    ENDIF.

  ENDMETHOD.


  METHOD verificar_bloqueio_equnr.

    r_v_bloqueado = abap_false.

    SELECT COUNT(*)
      FROM kssk
      WHERE objek = me->v_equnr
        AND klart = me->c_klart.

    IF sy-subrc IS INITIAL.

      r_v_bloqueado = abap_true.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
