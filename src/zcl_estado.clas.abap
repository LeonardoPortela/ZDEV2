class ZCL_ESTADO definition
  public
  final
  create public .

public section.

  interfaces ZIF_ESTADO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ESTADO IMPLEMENTATION.


  METHOD ZIF_ESTADO~GET_ID_BACEN.

    R_IF_ESTADO = ME.

    CLEAR: E_ID_BACEN.

    CASE I_UF.
      WHEN 'TO'. E_ID_BACEN = '17'.
      WHEN 'PE'. E_ID_BACEN = '26'.
      WHEN 'ES'. E_ID_BACEN = '32'.
      WHEN 'RS'. E_ID_BACEN = '43'.
      WHEN 'AC'. E_ID_BACEN = '12'.
      WHEN 'RO'. E_ID_BACEN = '11'.
      WHEN 'MA'. E_ID_BACEN = '21'.
      WHEN 'AL'. E_ID_BACEN = '27'.
      WHEN 'RJ'. E_ID_BACEN = '33'.
      WHEN 'MS'. E_ID_BACEN = '50'.
      WHEN 'AM'. E_ID_BACEN = '13'.
      WHEN 'PB'. E_ID_BACEN = '25'.
      WHEN 'PI'. E_ID_BACEN = '22'.
      WHEN 'SE'. E_ID_BACEN = '28'.
      WHEN 'SP'. E_ID_BACEN = '35'.
      WHEN 'MT'. E_ID_BACEN = '51'.
      WHEN 'RR'. E_ID_BACEN = '14'.
      WHEN 'CE'. E_ID_BACEN = '23'.
      WHEN 'BA'. E_ID_BACEN = '29'.
      WHEN 'PR'. E_ID_BACEN = '41'.
      WHEN 'GO'. E_ID_BACEN = '52'.
      WHEN 'AP'. E_ID_BACEN = '16'.
      WHEN 'RN'. E_ID_BACEN = '24'.
      WHEN 'MG'. E_ID_BACEN = '31'.
      WHEN 'SC'. E_ID_BACEN = '42'.
      WHEN 'DF'. E_ID_BACEN = '53'.
      WHEN 'PA'. E_ID_BACEN = '15'.
    ENDCASE.

  ENDMETHOD.


  METHOD ZIF_ESTADO~GET_INSTANCE.

    IF ZIF_ESTADO~AT_ESTADO IS NOT BOUND.
      CREATE OBJECT ZIF_ESTADO~AT_ESTADO TYPE ZCL_ESTADO.
    ENDIF.
    R_IF_ESTADO = ZIF_ESTADO~AT_ESTADO.

  ENDMETHOD.


  method ZIF_ESTADO~GET_SIGLA_ESTADO.

    FIELD-SYMBOLS <fs_model>  TYPE any.
    FIELD-SYMBOLS <fs_regio> TYPE any.

    R_IF_ESTADO = ME.

    CLEAR: E_UF.


    CASE I_ID_BACEN.
      WHEN '17'. E_UF = 'TO'.
      WHEN '26'. E_UF = 'PE'.
      WHEN '32'. E_UF = 'ES'.
      WHEN '43'. E_UF = 'RS'.
      WHEN '12'. E_UF = 'AC'.
      WHEN '11'. E_UF = 'RO'.
      WHEN '21'. E_UF = 'MA'.
      WHEN '27'. E_UF = 'AL'.
      WHEN '33'. E_UF = 'RJ'.
      WHEN '50'. E_UF = 'MS'.
      WHEN '13'. E_UF = 'AM'.
      WHEN '25'. E_UF = 'PB'.
      WHEN '22'. E_UF = 'PI'.
      WHEN '28'. E_UF = 'SE'.
      WHEN '35'. E_UF = 'SP'.
      WHEN '51'. E_UF = 'MT'.
      WHEN '14'. E_UF = 'RR'.
      WHEN '23'. E_UF = 'CE'.
      WHEN '29'. E_UF = 'BA'.
      WHEN '41'. E_UF = 'PR'.
      WHEN '52'. E_UF = 'GO'.
      WHEN '16'. E_UF = 'AP'.
      WHEN '24'. E_UF = 'RN'.
      WHEN '31'. E_UF = 'MG'.
      WHEN '42'. E_UF = 'SC'.
      WHEN '53'. E_UF = 'DF'.
      WHEN '15'. E_UF = 'PA'.
    ENDCASE.

**
**ASSIGN ('(Z_1BNFE_MONITOR)wa_nfe_alv-MODEL')  TO     <fs_model>.
**ASSIGN ('(Z_1BNFE_MONITOR)wa_nfe_alv-REGIO')  TO    <fs_regio> .
** IF  <fs_model> IS ASSIGNED.
**
**
**     IF  <fs_model>  = 57.
**        IF  <fs_model>  = 14.
**
**         E_UF = 'SP'.
**
**
**       ENDIF.
**     ENDIF.
**ENDIF.



  endmethod.
ENDCLASS.
