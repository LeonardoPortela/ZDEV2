class ZCL_FI_CONTAB_ARGENTINA definition
  public
  final
  create public .

public section.

  types:
    ty_arquivo(800) TYPE c,

    BEGIN OF TY_ZFIT0236,
      EIGR1 TYPE ZFIT0236-EIGR1,
    END OF TY_ZFIT0236.


  methods CONSTRUCTOR
    importing
      !I_W_FEBKO type FEBKO
      !I_V_ARQUIVO type TY_ARQUIVO .
  methods INICIAR
    changing
      !C_W_FEBEP type FEBEP .
protected section.
private section.

  data W_FEBKO type FEBKO .
  data V_TEXTO type ZFIT0236-TEXTO .
  data W_ZFIT0236 type TY_ZFIT0236 .

  methods VERIFICAR_VALIDO
    returning
      value(R_V_VALIDO) type BOOLEAN .
  methods VERIFICAR_CONTABILIZACAO
    raising
      CX_FIBR_CREDIT_CONTROL_ERROR .
  methods RESGATAR_TEXTO
    importing
      !I_V_ARQUIVO type TY_ARQUIVO .
ENDCLASS.



CLASS ZCL_FI_CONTAB_ARGENTINA IMPLEMENTATION.


  METHOD constructor.

    me->w_febko = i_w_febko.
    me->resgatar_texto( i_v_arquivo ).

  ENDMETHOD.


  METHOD iniciar.

    IF me->verificar_valido( ) IS NOT INITIAL.

      c_w_febep-vgint = me->w_zfit0236-eigr1.

    ENDIF.

  ENDMETHOD.


  METHOD resgatar_texto.

    DATA:
          lt_dados TYPE TABLE OF string.

    TRY.

        SPLIT i_v_arquivo AT ';'
                        INTO TABLE lt_dados.

        READ TABLE lt_dados INTO DATA(ls_dados) INDEX 17.
        IF sy-subrc IS INITIAL.

          me->v_texto = ls_dados.
          SHIFT me->v_texto LEFT DELETING LEADING space.

        ENDIF.

      CATCH cx_root.
        " Ocorreu algum erro ao acessar o dado...
    ENDTRY.


  ENDMETHOD.


  METHOD verificar_contabilizacao.

    DATA:
          lv_bankl TYPE zfit0236-bankl.

    SELECT SINGLE eigr1
      INTO me->w_zfit0236
      FROM zfit0236
      WHERE bukrs = me->w_febko-bukrs
        AND bankl = me->w_febko-absnd(3)
        AND texto = me->v_texto.

    IF sy-subrc IS NOT INITIAL.

      RAISE EXCEPTION TYPE cx_fibr_credit_control_error.

    ENDIF.


  ENDMETHOD.


  METHOD verificar_valido.

    TRY.

        r_v_valido = abap_true.

        me->verificar_contabilizacao( ).

      CATCH cx_root.

        r_v_valido = abap_false.

    ENDTRY.

  ENDMETHOD.
ENDCLASS.
