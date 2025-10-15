class ZCL_SHDB definition
  public
  final
  create public .

public section.

  interfaces ZIF_SHDB .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SHDB IMPLEMENTATION.


  METHOD ZIF_SHDB~GET_CK_EXISTE_MSG_ERRO.

    R_INSTANCIA = ME.
    E_MSG_TAB   = ME->ZIF_SHDB~AT_MSG.

    DESCRIBE TABLE ME->ZIF_SHDB~AT_MSG LINES DATA(QT_LINHAS_01).

    "DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGV1 CS 'foi criado'.

    "Foi criada a conta &1.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '170' AND MSGTYP = 'E'.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '170' AND MSGTYP = 'A'.

    "O cliente &1 foi criado na empresa &2.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '171' AND MSGTYP = 'E'.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '171' AND MSGTYP = 'A'.

    "O cliente &1 foi criado na área de vendas &2 &3 &4.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '172' AND MSGTYP = 'E'.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '172' AND MSGTYP = 'A'.

    "O cliente &1 foi criado na organização de compras &2.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '173' AND MSGTYP = 'E'.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '173' AND MSGTYP = 'A'.

    "Cliente &1 foi criado p/empresa &2 e área de vendas &3.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '174' AND MSGTYP = 'E'.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '174' AND MSGTYP = 'A'.

    "Cliente &1 foi criado p/empresa &2 e organização de compras &3.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '175' AND MSGTYP = 'E'.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '175' AND MSGTYP = 'A'.

    " O cliente &1 foi criado
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '247' AND MSGTYP = 'E'.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '247' AND MSGTYP = 'A'.

    "O fornecedor &1 foi criado
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '249' AND MSGTYP = 'E'.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '249' AND MSGTYP = 'A'.

    "O fornecedor &1 foi criado na empresa &2.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '271' AND MSGTYP = 'E'.
    DELETE ME->ZIF_SHDB~AT_MSG WHERE MSGID = 'F2' AND MSGNR = '271' AND MSGTYP = 'A'.

    DESCRIBE TABLE ME->ZIF_SHDB~AT_MSG LINES DATA(QT_LINHAS_02).

    IF QT_LINHAS_02 NE QT_LINHAS_01.
      RAISE EXCEPTION TYPE ZCX_SHDB
        EXPORTING
          TEXTID = VALUE #( MSGID = ZCX_SHDB=>ZCX_SEM_MSG_ERRO-MSGID
                            MSGNO = ZCX_SHDB=>ZCX_SEM_MSG_ERRO-MSGNO )
          MSGID  = ZCX_SHDB=>ZCX_SEM_MSG_ERRO-MSGID
          MSGNO  = ZCX_SHDB=>ZCX_SEM_MSG_ERRO-MSGNO
          MSGTY  = 'E'.
    ENDIF.

    READ TABLE ME->ZIF_SHDB~AT_MSG INTO E_MSG
    WITH KEY MSGTYP = 'E'.

    CHECK SY-SUBRC IS NOT INITIAL.

    READ TABLE ME->ZIF_SHDB~AT_MSG INTO E_MSG
    WITH KEY MSGTYP = 'A'.

    CHECK SY-SUBRC IS NOT INITIAL.

    RAISE EXCEPTION TYPE ZCX_SHDB
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_SHDB=>ZCX_SEM_MSG_ERRO-MSGID
                          MSGNO = ZCX_SHDB=>ZCX_SEM_MSG_ERRO-MSGNO )
        MSGID  = ZCX_SHDB=>ZCX_SEM_MSG_ERRO-MSGID
        MSGNO  = ZCX_SHDB=>ZCX_SEM_MSG_ERRO-MSGNO
        MSGTY  = 'E'.

  ENDMETHOD.


  METHOD ZIF_SHDB~GET_ERRO_GERAL.

    RAISE EXCEPTION TYPE ZCX_SHDB
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

  ENDMETHOD.


  METHOD ZIF_SHDB~GET_ERRO_GERAL_STRING.

    DATA: LC_TEXTO TYPE C LENGTH 200.
    LC_TEXTO = I_TEXTO.
    SY-MSGV1 = LC_TEXTO+000(50).
    SY-MSGV2 = LC_TEXTO+050(50).
    SY-MSGV3 = LC_TEXTO+100(50).
    SY-MSGV4 = LC_TEXTO+150(50).

    RAISE EXCEPTION TYPE ZCX_SHDB
      EXPORTING
        TEXTID = VALUE #( MSGID = ZCX_SHDB=>ZCX_ERRO_GERAL-MSGID
                          MSGNO = ZCX_SHDB=>ZCX_ERRO_GERAL-MSGNO
                          ATTR1 = CONV #( SY-MSGV1 )
                          ATTR2 = CONV #( SY-MSGV2 )
                          ATTR3 = CONV #( SY-MSGV3 )
                          ATTR4 = CONV #( SY-MSGV4 ) )
        MSGID  = ZCX_SHDB=>ZCX_ERRO_GERAL-MSGID
        MSGNO  = ZCX_SHDB=>ZCX_ERRO_GERAL-MSGNO
        MSGTY  = 'E'
        MSGV1  = SY-MSGV1
        MSGV2  = SY-MSGV2
        MSGV3  = SY-MSGV3
        MSGV4  = SY-MSGV4.

  ENDMETHOD.


  METHOD ZIF_SHDB~GET_INSTANCE.

    IF ZIF_SHDB~INSTANCE IS NOT BOUND.
      CREATE OBJECT ZIF_SHDB~INSTANCE TYPE ZCL_SHDB.
      R_INSTANCIA = ZIF_SHDB~INSTANCE.
    ELSE.
      R_INSTANCIA = ZIF_SHDB~INSTANCE.
    ENDIF.

    R_INSTANCIA->SET_CLEAR( ).

  ENDMETHOD.


  METHOD ZIF_SHDB~SET_ADD_BDCDATA.

    R_INSTANCIA = ME.
    APPEND I_BDCDATA TO ME->ZIF_SHDB~AT_SHDB.

  ENDMETHOD.


  METHOD ZIF_SHDB~SET_CLEAR.

    R_INSTANCIA = ME.

    CLEAR: ME->ZIF_SHDB~AT_MSG,
           ME->ZIF_SHDB~AT_SHDB,
           ME->ZIF_SHDB~AT_MODE,
           ME->ZIF_SHDB~AT_TRANSACTION.

    ME->ZIF_SHDB~AT_MODE = ZIF_SHDB=>ST_TIPO_MODE_SEM_TELA_SEM_DEBU.

  ENDMETHOD.


  METHOD zif_shdb~set_executar.

    r_instancia = me.
    DATA: lt_bdc    TYPE bdcdata_tab,
          lt_bdcmsg TYPE tab_bdcmsgcoll,
          wa_lfa1   TYPE lfa1.

    DATA: lo_bp TYPE REF TO zcl_bp_utils.

    READ TABLE  me->zif_shdb~at_shdb INTO DATA(_shdb) WITH KEY program = 'SAPMF02K'. "fornecedor chama BP
    IF sy-subrc EQ 0.
      lt_bdc = CONV #( me->zif_shdb~at_shdb[] ).

      CREATE OBJECT lo_bp
        EXPORTING
          im_test    = abap_false
          im_tcode   = 'BP'
          it_bdcdata = lt_bdc.

      CALL METHOD lo_bp->mt_bp_process_old_shdb(
        CHANGING
          ct_bdcmsg = lt_bdcmsg ).

      CALL METHOD lo_bp->mt_bp_process_data( CHANGING ct_bdcmsg = lt_bdcmsg ).

      me->zif_shdb~at_msg = CONV #( lt_bdcmsg[] ).

    ELSE.
      CALL TRANSACTION me->zif_shdb~at_transaction USING me->zif_shdb~at_shdb
            MODE me->zif_shdb~at_mode
            MESSAGES INTO me->zif_shdb~at_msg.
    ENDIF.


  ENDMETHOD.


  METHOD ZIF_SHDB~SET_MODE.

    R_INSTANCIA = ME.

    ME->ZIF_SHDB~AT_MODE = I_MODE.

  ENDMETHOD.


  METHOD ZIF_SHDB~SET_TRANSACTION.

    R_INSTANCIA = ME.

    ME->ZIF_SHDB~AT_TRANSACTION = I_TCODE.

  ENDMETHOD.
ENDCLASS.
