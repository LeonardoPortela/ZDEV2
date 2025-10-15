class ZCL_CHARG definition
  public
  final
  create public .

public section.

  class-methods GET_CARCATERISTICAS_CLASS
    importing
      !I_CLASS type KLASSE_D default ' '
      !I_CLASSTYPE type KLASSENART
      !I_OBJECT type OBJNUM
      !I_TABLE type TABELLE
    exporting
      !E_T_CLASS type Z_DE_SCLASS_T
    returning
      value(R_T_OBJECTDATA) type ZDE_CLOBJDAT_T
    raising
      ZCX_CHARG_EXCEPTION .
  class-methods GET_CHARG
    importing
      !I_MATNR type MATNR
      !I_CHARG type CHARG_D optional
      !I_LICHA type LICHN optional
    returning
      value(R_MCH1) type MCH1
    raising
      ZCX_CHARG_EXCEPTION .
  class-methods GET_CHARG_WERKS
    importing
      !I_MATNR type MATNR
      !I_CHARG type CHARG_D
      !I_WERKS type WERKS_D
    returning
      value(R_MCHA) type MCHA
    raising
      ZCX_CHARG_EXCEPTION .
  class-methods GET_CHARG_WERKS_DEPOSITO
    importing
      !I_MATNR type MATNR
      !I_CHARG type CHARG_D
      !I_WERKS type WERKS_D
      !I_LGORT type LGORT_D
    returning
      value(R_MCHB) type MCHB
    raising
      ZCX_CHARG_EXCEPTION .
  methods CRIAR_CHARG
    exporting
      !E_MCH1 type MCH1
    returning
      value(R_MCHB) type MCHB
    raising
      ZCX_CHARG_EXCEPTION .
  methods SET_MCHA
    importing
      !I_MCHA type MCHA .
  methods SET_LGORT
    importing
      !I_LGORT type LGORT_D .
  methods SET_KLART
    importing
      !I_KLART type KLASSENART .
  methods SET_CLASS
    importing
      !I_CLASS type KLASSE_D .
  methods SET_OBJECT
    importing
      !I_OBJECT type OBJNUM .
  methods SET_KZCLA
    importing
      !I_KZCLA type KZCLA .
  methods SET_TABLE
    importing
      !I_TABLE type TABELLE .
  methods ADD_CLBATCH
    importing
      !I_CLASS type KLAH-CLASS optional
      !I_CLASSTYPE type KLAH-KLART optional
      !I_CLBATCH type CLBATCH
    raising
      ZCX_CHARG_EXCEPTION .
  class-methods VALIDA_VALOR_CARACTERISTICA
    importing
      !I_CLASS type KLAH-CLASS
      !I_CLASSTYPE type KLAH-KLART
      !I_ATINN type ATINN
      !I_ATWRT type ATWRT
    raising
      ZCX_CHARG_EXCEPTION .
  class-methods GET_CHARG_DETALHE
    importing
      !I_MATNR type MARA-MATNR
      !I_CHARG type MCH1-CHARG
      !I_WERKS type T001W-WERKS optional
    exporting
      !E_YMCHA type MCHA
      !E_CLASSNAME type KLAH-CLASS
    returning
      value(R_CHAR_OF_BATCH) type CLBATCH_T
    raising
      ZCX_CHARG_EXCEPTION .
  methods GET_RETORNO
    returning
      value(R_RETORNO) type BAPIRET2_T .
  class-methods GET_VALOR_CARACTERISTICA
    importing
      !IV_NOME_ATTR type STRING
      !IV_MATNR type MARA-MATNR
      !IV_CHARG type J_1BNFLIN-CHARG
    exporting
      !E_ATWRT type ATWRT
    returning
      value(R_VALOR) type STRING .
protected section.
private section.

  data MCHA type MCHA .
  data LGORT type LGORT_D .
  data KLART type KLASSENART .
  data CLASS type KLASSE_D .
  data OBJECT type OBJNUM .
  data KZCLA type KZCLA .
  data CLBATCH type CLBATCH_T .
  data TABLE type TABELLE .
  data AT_RETORNO type BAPIRET2_T .
ENDCLASS.



CLASS ZCL_CHARG IMPLEMENTATION.


  METHOD ADD_CLBATCH.

    DATA: LC_CLBATCH   TYPE CLBATCH,
          LC_CLASS     TYPE KLAH-CLASS,
          LC_CLASSTYPE TYPE KLAH-KLART.

    MOVE I_CLBATCH TO LC_CLBATCH.

    IF I_CLASS IS INITIAL.
      LC_CLASS = ME->CLASS.
    ELSE.
      LC_CLASS = I_CLASS.
    ENDIF.

    IF I_CLASSTYPE IS INITIAL.
      LC_CLASSTYPE = ME->KLART.
    ELSE.
      LC_CLASSTYPE = I_CLASSTYPE.
    ENDIF.

    IF LC_CLASS IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_TIPO_CLASSE-MSGID
                            MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_TIPO_CLASSE-MSGNO )
          MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_TIPO_CLASSE-MSGID
          MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_TIPO_CLASSE-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF LC_CLASSTYPE IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_CLASSE-MSGID
                            MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_CLASSE-MSGNO )
          MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_CLASSE-MSGID
          MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_CLASSE-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF LC_CLBATCH-ATNAM IS NOT INITIAL AND LC_CLBATCH-ATINN IS INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
        EXPORTING
          INPUT  = LC_CLBATCH-ATNAM
        IMPORTING
          OUTPUT = LC_CLBATCH-ATINN.

    ELSEIF LC_CLBATCH-ATINN IS NOT INITIAL AND LC_CLBATCH-ATNAM IS INITIAL.

      CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
        EXPORTING
          INPUT  = LC_CLBATCH-ATINN
        IMPORTING
          OUTPUT = LC_CLBATCH-ATNAM.

    ENDIF.

    IF LC_CLBATCH-ATWTB IS NOT INITIAL.
      CALL METHOD ZCL_CHARG=>VALIDA_VALOR_CARACTERISTICA
        EXPORTING
          I_CLASS     = LC_CLASS
          I_CLASSTYPE = LC_CLASSTYPE
          I_ATINN     = LC_CLBATCH-ATINN
          I_ATWRT     = LC_CLBATCH-ATWTB.
    ENDIF.

    APPEND LC_CLBATCH TO ME->CLBATCH.

  ENDMETHOD.


  METHOD CRIAR_CHARG.

    DATA: IT_RETURN        TYPE TABLE OF BAPIRET2,
          IT_NEW_BATCH     TYPE TABLE OF MCHA,
          IT_CHAR_OF_BATCH TYPE TABLE OF CLBATCH.

    IF ME->MCHA-CHARG IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_NR_LOTE-MSGID
                            MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_NR_LOTE-MSGNO )
          MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_NR_LOTE-MSGID
          MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_NR_LOTE-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF ME->MCHA-MATNR IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_MATERIAL-MSGID
                            MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_MATERIAL-MSGNO )
          MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_MATERIAL-MSGID
          MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_MATERIAL-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF ME->MCHA-WERKS IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_CENTRO-MSGID
                            MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_CENTRO-MSGNO )
          MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_CENTRO-MSGID
          MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_CENTRO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

*    IF ME->MCHA-VFDAT IS INITIAL.
*      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
*        EXPORTING
*          TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_DT_VENCIMENTO-MSGID
*                            MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_DT_VENCIMENTO-MSGNO )
*          MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_DT_VENCIMENTO-MSGID
*          MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_DT_VENCIMENTO-MSGNO
*          MSGTY  = 'E'.
*    ENDIF.

    IF ME->LGORT IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_DEPOSITO-MSGID
                            MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_DEPOSITO-MSGNO )
          MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_DEPOSITO-MSGID
          MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_DEPOSITO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF ME->CLASS IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_TIPO_CLASSE-MSGID
                            MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_TIPO_CLASSE-MSGNO )
          MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_TIPO_CLASSE-MSGID
          MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_TIPO_CLASSE-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF ME->KLART IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_CLASSE-MSGID
                            MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_CLASSE-MSGNO )
          MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_CLASSE-MSGID
          MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_CLASSE-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF ME->OBJECT IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_OBJETO-MSGID
                            MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_OBJETO-MSGNO )
          MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_OBJETO-MSGID
          MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_OBJETO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF ME->TABLE IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_TABELA-MSGID
                            MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_TABELA-MSGNO )
          MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_TABELA-MSGID
          MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_TABELA-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    IF ME->KZCLA IS INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_POSS_CLASS-MSGID
                            MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_POSS_CLASS-MSGNO )
          MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_POSS_CLASS-MSGID
          MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_INFORMAR_POSS_CLASS-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    CALL METHOD ZCL_CHARG=>GET_CARCATERISTICAS_CLASS
      EXPORTING
        I_CLASS        = ME->CLASS
        I_CLASSTYPE    = ME->KLART
        I_OBJECT       = ME->OBJECT
        I_TABLE        = ME->TABLE
*      IMPORTING
*       E_T_CLASS      =
      RECEIVING
        R_T_OBJECTDATA = DATA(IT_OBJETOS).

    SORT IT_OBJETOS BY ATNAM.

    CLEAR: IT_CHAR_OF_BATCH.

    LOOP AT ME->CLBATCH INTO DATA(WA_CLBATCH).
      READ TABLE IT_OBJETOS WITH KEY ATNAM = WA_CLBATCH-ATNAM TRANSPORTING NO FIELDS BINARY SEARCH.
      IF SY-SUBRC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_ERRO_CARACTERISTICA-MSGID
                              MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_ERRO_CARACTERISTICA-MSGNO
                              ATTR1 = CONV #( WA_CLBATCH-ATNAM )
                              ATTR2 = CONV #( ME->CLASS ) )
            MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_ERRO_CARACTERISTICA-MSGID
            MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_ERRO_CARACTERISTICA-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( WA_CLBATCH-ATNAM )
            MSGV2  = CONV #( ME->CLASS ).
      ENDIF.

      IF WA_CLBATCH-ATWTB IS NOT INITIAL.
        CALL METHOD ZCL_CHARG=>VALIDA_VALOR_CARACTERISTICA
          EXPORTING
            I_CLASS     = ME->CLASS
            I_CLASSTYPE = ME->KLART
            I_ATINN     = WA_CLBATCH-ATINN
            I_ATWRT     = WA_CLBATCH-ATWTB.

        APPEND WA_CLBATCH TO IT_CHAR_OF_BATCH.
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'VB_CREATE_BATCH'
      EXPORTING
        YMCHA                        = ME->MCHA
        NEW_LGORT                    = ME->LGORT
*       BYPASS_LOCK                  = ' '
*       MESSAGE_WHEN_AUTO            = ' '
*       X_BNCOM                      =
*       QM_ZUSTD                     =
*       REF_MATNR                    =
*       REF_CHARG                    =
*       REF_WERKS                    =
        KZCLA                        = ME->KZCLA
*       XKCFC                        =
        CLASS                        = ME->CLASS
*       NO_CHECK_OF_QM_CHAR          = ' '
*       BUFFER_REFRESH               = ' '
*       NO_CHANGE_DOCUMENT           = ' '
*       NO_CFC_CALLS                 = 'X'
*       NO_CHANGE_OF_BWTAR           = ' '
*     IMPORTING
*       YMCHA                        =
      TABLES
        CHAR_OF_BATCH                = IT_CHAR_OF_BATCH
        NEW_BATCH                    = IT_NEW_BATCH
*       NEW_BATCH_STOLOC             =
        RETURN                       = IT_RETURN
      EXCEPTIONS
        NO_MATERIAL                  = 1
        NO_BATCH                     = 2
        NO_PLANT                     = 3
        MATERIAL_NOT_FOUND           = 4
        PLANT_NOT_FOUND              = 5
        STOLOC_NOT_FOUND             = 6
        LOCK_ON_MATERIAL             = 7
        LOCK_ON_PLANT                = 8
        LOCK_ON_BATCH                = 9
        LOCK_SYSTEM_ERROR            = 10
        NO_AUTHORITY                 = 11
        BATCH_EXIST                  = 12
        STOLOC_EXIST                 = 13
        ILLEGAL_BATCH_NUMBER         = 14
        NO_BATCH_HANDLING            = 15
        NO_VALUATION_AREA            = 16
        VALUATION_TYPE_NOT_FOUND     = 17
        NO_VALUATION_FOUND           = 18
        ERROR_AUTOMATIC_BATCH_NUMBER = 19
        CANCELLED                    = 20
        WRONG_STATUS                 = 21
        INTERVAL_NOT_FOUND           = 22
        NUMBER_RANGE_NOT_EXTERN      = 23
        OBJECT_NOT_FOUND             = 24
        ERROR_CHECK_BATCH_NUMBER     = 25
        NO_EXTERNAL_NUMBER           = 26
        NO_CUSTOMER_NUMBER           = 27
        NO_CLASS                     = 28
        ERROR_IN_CLASSIFICATION      = 29
        INCONSISTENCY_IN_KEY         = 30
        REGION_OF_ORIGIN_NOT_FOUND   = 31
        COUNTRY_OF_ORIGIN_NOT_FOUND  = 32
        OTHERS                       = 33.

    IF SY-SUBRC IS NOT INITIAL.

      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.

      MOVE IT_RETURN[] TO ME->AT_RETORNO[].

      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID
                            MSGNO = SY-MSGNO
                            ATTR1 = CONV #( SY-MSGV1 )
                            ATTR2 = CONV #( SY-MSGV2 )
                            ATTR3 = CONV #( SY-MSGV3 )
                            ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGNO  = SY-MSGNO
          MSGTY  = 'E'
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ELSE.

      MOVE IT_RETURN[] TO ME->AT_RETORNO[].

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.

      "Procura Lote Gerado
      SELECT SINGLE * INTO @DATA(WA_MCH1)
        FROM MCH1
       WHERE MATNR = @ME->MCHA-MATNR
         AND CHARG = @ME->MCHA-CHARG.

      IF SY-SUBRC IS NOT INITIAL.
        WAIT UP TO 4 SECONDS.
        SELECT SINGLE * INTO WA_MCH1
          FROM MCH1
         WHERE MATNR = ME->MCHA-MATNR
           AND CHARG = ME->MCHA-CHARG.
        IF SY-SUBRC IS NOT INITIAL.
          WAIT UP TO 4 SECONDS.
          SELECT SINGLE * INTO WA_MCH1
            FROM MCH1
           WHERE MATNR = ME->MCHA-MATNR
             AND CHARG = ME->MCHA-CHARG.
        ENDIF.

      ENDIF.

      IF SY-SUBRC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_ERRO_CRIA_LOTE-MSGID
                              MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_ERRO_CRIA_LOTE-MSGNO
                              ATTR1 = CONV #( ME->MCHA-MATNR )
                              ATTR2 = CONV #( ME->MCHA-CHARG ) )
            MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_ERRO_CRIA_LOTE-MSGID
            MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_ERRO_CRIA_LOTE-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( ME->MCHA-MATNR )
            MSGV2  = CONV #( ME->MCHA-CHARG ).
      ELSE.
        E_MCH1 = WA_MCH1.
      ENDIF.

      SELECT SINGLE * INTO @DATA(WA_MCHB)
        FROM MCHB
       WHERE MATNR = @ME->MCHA-MATNR
         AND CHARG = @ME->MCHA-CHARG
         AND WERKS = @ME->MCHA-WERKS
         AND LGORT = @ME->LGORT.

      IF SY-SUBRC IS NOT INITIAL.
        RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
          EXPORTING
            TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_ERRO_CRIA_LOTE_DEPOSITO-MSGID
                              MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_ERRO_CRIA_LOTE_DEPOSITO-MSGNO
                              ATTR1 = CONV #( ME->MCHA-MATNR )
                              ATTR2 = CONV #( ME->MCHA-CHARG )
                              ATTR3 = CONV #( ME->MCHA-WERKS )
                              ATTR4 = CONV #( ME->LGORT ) )
            MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_ERRO_CRIA_LOTE_DEPOSITO-MSGID
            MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_ERRO_CRIA_LOTE_DEPOSITO-MSGNO
            MSGTY  = 'E'
            MSGV1  = CONV #( ME->MCHA-MATNR )
            MSGV2  = CONV #( ME->MCHA-CHARG )
            MSGV3  = CONV #( ME->MCHA-WERKS )
            MSGV4  = CONV #( ME->LGORT ).
      ELSE.
        R_MCHB = WA_MCHB.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD GET_CARCATERISTICAS_CLASS.

    CLEAR:
    E_T_CLASS,
    R_T_OBJECTDATA.

    data I_OBJECT90 type CUOBN.
    I_OBJECT90 = I_OBJECT.

    CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS' "#EC CI_FLDEXT_OK[2215424]
      EXPORTING
        CLASS              = I_CLASS
*       CLASSTEXT          = 'X'
        CLASSTYPE          = I_CLASSTYPE
*       CLINT              = 0
*       FEATURES           = 'X'
*       LANGUAGE           = SY-LANGU
        OBJECT             = I_OBJECT90 "#EC CI_FLDEXT_OK[2215424]
        OBJECTTABLE        = I_TABLE
*       KEY_DATE           = SY-DATUM
*       INITIAL_CHARACT    = 'X'
*       NO_VALUE_DESCRIPT  =
*       CHANGE_SERVICE_CLF = 'X'
*       INHERITED_CHAR     = ' '
*       CHANGE_NUMBER      = ' '
      TABLES
        T_CLASS            = E_T_CLASS
        T_OBJECTDATA       = R_T_OBJECTDATA
*       I_SEL_CHARACTERISTIC       =
*       T_NO_AUTH_CHARACT  =
      EXCEPTIONS
        NO_CLASSIFICATION  = 1
        NO_CLASSTYPES      = 2
        INVALID_CLASS_TYPE = 3
        OTHERS             = 4.

    IF SY-SUBRC IS NOT INITIAL.
      CASE SY-SUBRC.
        WHEN 1.
          RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_CLASSE_NOT_FOUND-MSGID
                                MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_CLASSE_NOT_FOUND-MSGNO
                                ATTR1 = CONV #( I_CLASS ) )
              MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_CLASSE_NOT_FOUND-MSGID
              MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_CLASSE_NOT_FOUND-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( I_CLASS ).
        WHEN 2.
          RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_TYPE_CLASSE_NOT_FOUND-MSGID
                                MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_TYPE_CLASSE_NOT_FOUND-MSGNO
                                ATTR1 = CONV #( I_CLASSTYPE ) )
              MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_TYPE_CLASSE_NOT_FOUND-MSGID
              MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_TYPE_CLASSE_NOT_FOUND-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( I_CLASSTYPE ).
        WHEN 3.
          RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
            EXPORTING
              TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_CLASSE_INVALIDA-MSGID
                                MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_CLASSE_INVALIDA-MSGNO
                                ATTR1 = CONV #( I_CLASS )
                                ATTR2 = CONV #( I_CLASSTYPE ) )
              MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_CLASSE_INVALIDA-MSGID
              MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_CLASSE_INVALIDA-MSGNO
              MSGTY  = 'E'
              MSGV1  = CONV #( I_CLASS )
              MSGV2  = CONV #( I_CLASSTYPE ).
        WHEN OTHERS.
          RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
            EXPORTING
              MSGTY = SY-MSGTY
              MSGNO = SY-MSGNO
              MSGV1 = SY-MSGV1
              MSGV2 = SY-MSGV2
              MSGV3 = SY-MSGV3
              MSGV4 = SY-MSGV4
              MSGID = SY-MSGID.
      ENDCASE.
    ENDIF.


  ENDMETHOD.


  METHOD get_charg.

    DATA: lv_charg TYPE charg_d.

*-CS2025000249-07.05.2025-#174157-JT-inicio
    IF i_licha IS NOT INITIAL.
      SELECT SINGLE * INTO r_mch1
        FROM mch1
       WHERE matnr EQ i_matnr
         AND licha EQ i_licha.
      lv_charg = i_licha.
    ELSE.
*-CS2025000249-07.05.2025-#174157-JT-fim
      SELECT SINGLE * INTO r_mch1
        FROM mch1
       WHERE matnr EQ i_matnr
         AND charg EQ i_charg.
      lv_charg = i_charg.
    ENDIF.

    IF sy-subrc IS NOT INITIAL.
      RAISE EXCEPTION TYPE zcx_charg_exception
        EXPORTING
          textid = VALUE #( msgid = zcx_charg_exception=>zcx_nao_ex_lote_material-msgid
                            msgno = zcx_charg_exception=>zcx_nao_ex_lote_material-msgno
                            attr1 = CONV #( lv_charg ) "CONV #( i_charg )  "*-CS2025000249-07.05.2025-#174157-JT
                            attr2 = CONV #( i_matnr )
                             )
          msgid  = zcx_charg_exception=>zcx_nao_ex_lote_material-msgid
          msgty  = 'E'
          msgno  = zcx_charg_exception=>zcx_nao_ex_lote_material-msgno
          msgv1  = CONV #( lv_charg ) "CONV #( i_charg )   "*-CS2025000249-07.05.2025-#174157-JT
          msgv2  = CONV #( i_matnr ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_CHARG_DETALHE.

    CALL FUNCTION 'VB_BATCH_GET_DETAIL'
      EXPORTING
        MATNR              = I_MATNR
        CHARG              = I_CHARG
        WERKS              = I_WERKS
        GET_CLASSIFICATION = ABAP_TRUE
*       EXISTENCE_CHECK    =
*       READ_FROM_BUFFER   =
*       NO_CLASS_INIT      = ' '
*       LOCK_BATCH         = ' '
      IMPORTING
        YMCHA              = E_YMCHA
        CLASSNAME          = E_CLASSNAME
      TABLES
        CHAR_OF_BATCH      = R_CHAR_OF_BATCH
      EXCEPTIONS
        NO_MATERIAL        = 1
        NO_BATCH           = 2
        NO_PLANT           = 3
        MATERIAL_NOT_FOUND = 4
        PLANT_NOT_FOUND    = 5
        NO_AUTHORITY       = 6
        BATCH_NOT_EXIST    = 7
        LOCK_ON_BATCH      = 8
        OTHERS             = 9.

    IF SY-SUBRC IS  NOT INITIAL.

      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID
                            MSGNO = SY-MSGNO
                            ATTR1 = CONV #( SY-MSGV1 )
                            ATTR2 = CONV #( SY-MSGV2 )
                            ATTR3 = CONV #( SY-MSGV3 )
                            ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGTY  = SY-MSGTY
          MSGNO  = SY-MSGNO
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.

    ENDIF.

  ENDMETHOD.


  METHOD GET_CHARG_WERKS.

    SELECT SINGLE * INTO R_MCHA
      FROM MCHA
     WHERE MATNR EQ I_MATNR
       AND CHARG EQ I_CHARG
       AND WERKS EQ I_WERKS.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_NAO_EX_LOTE_MATERIALW-MSGID
                            MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_NAO_EX_LOTE_MATERIALW-MSGNO
                            ATTR1 = CONV #( I_CHARG )
                            ATTR2 = CONV #( I_MATNR )
                            ATTR3 = CONV #( I_WERKS )
                             )
          MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_NAO_EX_LOTE_MATERIALW-MSGID
          MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_NAO_EX_LOTE_MATERIALW-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_CHARG )
          MSGV2  = CONV #( I_MATNR )
          MSGV3  = CONV #( I_WERKS ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_CHARG_WERKS_DEPOSITO.

    SELECT SINGLE * INTO R_MCHB
      FROM MCHB
     WHERE MATNR EQ I_MATNR
       AND CHARG EQ I_CHARG
       AND WERKS EQ I_WERKS
       AND LGORT EQ I_LGORT.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_CHARG_EXCEPTION=>ZCX_NAO_EX_LOTE_MATERIALWD-MSGID
                            MSGNO = ZCX_CHARG_EXCEPTION=>ZCX_NAO_EX_LOTE_MATERIALWD-MSGNO
                            ATTR1 = CONV #( I_CHARG )
                            ATTR2 = CONV #( I_MATNR )
                            ATTR3 = CONV #( I_WERKS )
                            ATTR4 = CONV #( I_LGORT )
                             )
          MSGID  = ZCX_CHARG_EXCEPTION=>ZCX_NAO_EX_LOTE_MATERIALWD-MSGID
          MSGNO  = ZCX_CHARG_EXCEPTION=>ZCX_NAO_EX_LOTE_MATERIALWD-MSGNO
          MSGTY  = 'E'
          MSGV1  = CONV #( I_CHARG )
          MSGV2  = CONV #( I_MATNR )
          MSGV3  = CONV #( I_WERKS )
          MSGV4  = CONV #( I_LGORT ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_RETORNO.
    R_RETORNO = ME->AT_RETORNO.
  ENDMETHOD.


  METHOD SET_CLASS.
    ME->CLASS = I_CLASS.
  ENDMETHOD.


  METHOD SET_KLART.
    ME->KLART = I_KLART.
  ENDMETHOD.


  METHOD SET_KZCLA.
    ME->KZCLA = I_KZCLA.
  ENDMETHOD.


  METHOD SET_LGORT.
    ME->LGORT = I_LGORT.
  ENDMETHOD.


  METHOD SET_MCHA.
    ME->MCHA = I_MCHA.
  ENDMETHOD.


  METHOD SET_OBJECT.
    ME->OBJECT = I_OBJECT.
  ENDMETHOD.


  METHOD SET_TABLE.
    ME->TABLE = I_TABLE.
  ENDMETHOD.


  METHOD VALIDA_VALOR_CARACTERISTICA.

*ATINN  1 Tipo  ATINN NUMC  10  0 Característica interna
*ATNAM  1 Tipo  ATNAM CHAR  30  0 Nome da característica
*ATWTB  1 Tipo  ATWTB CHAR  30  0 Denominação do valor da característica

*ATIMB  1 Tipo  ATIMB NUMC  10  0 Nº característica da ctg.dados definida pelo usuário
*ATZIS  1 Tipo  ATZIS NUMC  3 0 Contador de instâncias

    DATA: IT_IMP_VALUES TYPE TABLE OF API_VAL_I,
          WA_IMP_VALUES TYPE API_VAL_I.

    WA_IMP_VALUES-ATINN = I_ATINN.

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
      EXPORTING
        INPUT  = I_ATINN
      IMPORTING
        OUTPUT = WA_IMP_VALUES-ATNAM.

    WA_IMP_VALUES-ATWTB = I_ATWRT.

    APPEND WA_IMP_VALUES TO IT_IMP_VALUES.

    CALL FUNCTION 'CTMS_CLASS_DDB'
      EXPORTING
        CLASS     = I_CLASS
        CLASSTYPE = I_CLASSTYPE
      EXCEPTIONS
        NOT_FOUND = 1
        OTHERS    = 2.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID
                            MSGNO = SY-MSGNO
                            ATTR1 = CONV #( SY-MSGV1 )
                            ATTR2 = CONV #( SY-MSGV2 )
                            ATTR3 = CONV #( SY-MSGV3 )
                            ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGTY  = SY-MSGTY
          MSGNO  = SY-MSGNO
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

    CALL FUNCTION 'CTMS_DDB_SET_VALUE'
      TABLES
        IMP_VALUES                    = IT_IMP_VALUES
      EXCEPTIONS
        CURRENCY_CHECK                = 1
        DATE_CHECK                    = 2
        FORMAT_CHECK                  = 3
        ILLEGAL_INTERNAL_BASEUNIT     = 4
        INTERVAL_CHECK                = 5
        PATTERN_CHECK                 = 6
        TIME_CHECK                    = 7
        UNIT_CHECK                    = 8
        VALUE_NOT_FOUND               = 9
        NO_VALID_DIMENSION            = 10
        INTERVAL_NOT_ALLOWED          = 11
        DISPLAY_MODE                  = 12
        CHARACTERISTIC_NOT_FOUND      = 13
        VALUE_NOT_POSSIBLE            = 14
        CHARACTERISTIC_ENQUEUE        = 15
        OBJECTCHARACTERISTIC          = 16
        ONLY_ONE_VALUE_ALLOWED        = 17
        CHARACTERISTIC_NOT_SELECTABLE = 18
        INPUT_TO_LONG                 = 19
        VALUE_CONTRADICTION           = 20
        OTHERS                        = 21.

    IF SY-SUBRC IS NOT INITIAL.
      RAISE EXCEPTION TYPE ZCX_CHARG_EXCEPTION
        EXPORTING
          TEXTID = VALUE #( MSGID = SY-MSGID
                            MSGNO = SY-MSGNO
                            ATTR1 = CONV #( SY-MSGV1 )
                            ATTR2 = CONV #( SY-MSGV2 )
                            ATTR3 = CONV #( SY-MSGV3 )
                            ATTR4 = CONV #( SY-MSGV4 ) )
          MSGID  = SY-MSGID
          MSGTY  = SY-MSGTY
          MSGNO  = SY-MSGNO
          MSGV1  = SY-MSGV1
          MSGV2  = SY-MSGV2
          MSGV3  = SY-MSGV3
          MSGV4  = SY-MSGV4.
    ENDIF.

  ENDMETHOD.


  METHOD get_valor_caracteristica.

    CLEAR: r_valor, e_atwrt.

    SELECT SINGLE cuobj_bm
      FROM mch1 INTO @DATA(v_cuobj_bm)
     WHERE matnr EQ @iv_matnr
       AND charg  EQ @iv_charg.

    CHECK sy-subrc EQ 0.

    SELECT SINGLE atinn
      FROM cabn INTO @DATA(v_atinn)
     WHERE atnam EQ @iv_nome_attr.

    CHECK sy-subrc EQ 0.

    CHECK v_cuobj_bm IS NOT INITIAL AND v_atinn IS NOT INITIAL.

    SELECT SINGLE *
     FROM ausp INTO @DATA(lwa_ausp)
    WHERE objek EQ @v_cuobj_bm
      AND atinn EQ @v_atinn
      AND klart EQ '023'.

    CHECK sy-subrc EQ 0.

    r_valor = lwa_ausp-dec_value_from.
    e_atwrt = lwa_ausp-atwrt.

  ENDMETHOD.
ENDCLASS.
