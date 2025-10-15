class ZCL_MM_DEPARTAMENTO definition
  public
  final
  create public .

public section.

  interfaces ZIF_CADASTRO .
  interfaces ZIF_PESQUISA .

  aliases EXCLUIR_REGISTRO
    for ZIF_CADASTRO~EXCLUIR_REGISTRO .
  aliases GET_REGISTRO
    for ZIF_CADASTRO~GET_REGISTRO .
  aliases GRAVAR_REGISTRO
    for ZIF_CADASTRO~GRAVAR_REGISTRO .
  aliases LIMPAR_REGISTRO
    for ZIF_CADASTRO~LIMPAR_REGISTRO .
  aliases NOVO_REGISTRO
    for ZIF_CADASTRO~NOVO_REGISTRO .
  aliases PESQUISAR
    for ZIF_PESQUISA~PESQUISAR .
  aliases SET_REGISTRO
    for ZIF_CADASTRO~SET_REGISTRO .
  aliases VALIDAR_EXCLUSAO
    for ZIF_CADASTRO~VALIDAR_EXCLUSAO .
  aliases VALIDAR_REGISTRO
    for ZIF_CADASTRO~VALIDAR_REGISTRO .
  aliases VALIDA_ATRIBUTO_ALTERAVEL
    for ZIF_CADASTRO~VALIDA_ATRIBUTO_ALTERAVEL .

  data CK_GRAVAR_ZMMT0210 type CHAR01 .
  data CK_GRAVAR_ZMMT0211 type CHAR01 .
  data CK_GRAVAR_ZMMT0213 type CHAR01 .
  data CK_ACAO_GRAVAR type CHAR01 .

  methods GET_CD_DEPARTAMENTO
    returning
      value(R_CD_DEPARTAMENTO) type ZDE_DEPARTAMENTO .
  methods GET_DS_DEPARTAMENTO
    returning
      value(R_DS_DEPARTAMENTO) type ZDE_DS_DEPARTAMENTO .
  methods GET_GRUPO_MERCADORIA
    returning
      value(R_GRUPOS) type ZDE_ZMMT0073_T .
  methods SET_DS_DEPARTAMENTO
    importing
      !I_DS_DEPARTAMENTO type ZDE_DS_DEPARTAMENTO .
  methods SET_CD_DEPARTAMENTO
    importing
      !I_CD_DEPARTAMENTO type ZDE_DEPARTAMENTO .
  methods SET_GRUPO_MERCADORIA
    importing
      !I_GRUPOS type ZDE_ZMMT0073_T .
  methods GET_TIPO_PEDIDO_COMPRA
    returning
      value(R_TIPOS) type ZDE_ZMMT0075_T .
  methods SET_TIPO_PEDIDO_COMPRA
    importing
      !I_TIPOS type ZDE_ZMMT0075_T .
  methods GET_ARMAZENS
    returning
      value(R_ARMAZENS) type ZDE_ZMMT0076_T .
  methods GET_CFOPS
    returning
      value(R_CFOPS) type ZDE_ZMMT0077_T .
  methods SET_ARMAZENS
    importing
      !I_ARMAZENS type ZDE_ZMMT0076_T .
  methods SET_CFOPS
    importing
      !I_CFOPS type ZDE_ZMMT0077_T .
  methods SET_EMPRESA_TIPO_PEDIDO
    importing
      !I_EMPRESA_TIPO_PEDIDO type ZDE_ZMMT0210_T
      !I_ACAO_GRAVAR type CHAR01 .
  methods SET_CFOP_MIRO
    importing
      !I_CFOP_MIRO type ZDE_ZMMT0211_T
      !I_ACAO_GRAVAR type CHAR01 .
  methods SET_EMPRESA_CLASSE
    importing
      !I_EMPRESA_CLASSE type ZDE_ZMMT0213_T
      !I_ACAO_GRAVAR type CHAR01 .
  methods CONSTRUCTOR
    importing
      !I_CD_DEPARTAMENTO type ZDE_DEPARTAMENTO optional .
  methods SET_CK_SEM_RET_GRUPO
    importing
      !I_CK_SEM_RET_GRUPO type ZDE_CK_SEM_RET_GRUPO .
  methods SET_CK_SEM_RET_PEDIDO
    importing
      !I_CK_SEM_RET_PEDIDO type ZDE_CK_SEM_RET_PEDIDO .
  methods GET_CK_SEM_RET_GRUPO
    returning
      value(R_CK_SEM_RET_GRUPO) type ZDE_CK_SEM_RET_GRUPO .
  methods GET_CK_SEM_RET_PEDIDO
    returning
      value(R_CK_SEM_RET_PEDIDO) type ZDE_CK_SEM_RET_PEDIDO .
  methods GET_CFOPS_RETORNO
    returning
      value(R_CFOPS) type ZDE_ZMMT0119_T .
  methods SET_CFOPS_RETORNO
    importing
      !I_CFOPS type ZDE_ZMMT0119_T .
protected section.
private section.

  aliases CK_ALTEROU
    for ZIF_CADASTRO~CK_ALTEROU .

  data CD_DEPARTAMENTO type ZDE_DEPARTAMENTO .
  data DS_DEPARTAMENTO type ZDE_DS_DEPARTAMENTO .
  data CK_SEM_RET_GRUPO type ZDE_CK_SEM_RET_GRUPO .
  data CK_SEM_RET_PEDIDO type ZDE_CK_SEM_RET_PEDIDO .
  data GRUPO_MERCADORIA type ZDE_ZMMT0073_T .
  data TIPO_PEDIDO_COMPRA type ZDE_ZMMT0075_T .
  data ARMAZENS type ZDE_ZMMT0076_T .
  data CFOPS type ZDE_ZMMT0077_T .
  data CFOPS_RETORNO type ZDE_ZMMT0119_T .
  data EMPRESA_TIPO_PEDIDO type ZDE_ZMMT0210_T .
  data CFOP_MIRO type ZDE_ZMMT0211_T .
  data EMPRESA_CLASSE type ZDE_ZMMT0213_T .
ENDCLASS.



CLASS ZCL_MM_DEPARTAMENTO IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    CALL METHOD SUPER->CONSTRUCTOR.

    ME->LIMPAR_REGISTRO( ).

    IF I_CD_DEPARTAMENTO IS NOT INITIAL.
      ME->SET_REGISTRO( EXPORTING I_ID_REGISTRO = I_CD_DEPARTAMENTO ).
    ENDIF.

  ENDMETHOD.


  method GET_ARMAZENS.
    R_ARMAZENS = ME->ARMAZENS.
  endmethod.


  METHOD GET_CD_DEPARTAMENTO.
    R_CD_DEPARTAMENTO = ME->CD_DEPARTAMENTO.
  ENDMETHOD.


  method GET_CFOPS.
    R_CFOPS = ME->CFOPS.
  endmethod.


  METHOD GET_CFOPS_RETORNO.

    R_CFOPS = ME->CFOPS_RETORNO.

  ENDMETHOD.


  METHOD GET_CK_SEM_RET_GRUPO.
    R_CK_SEM_RET_GRUPO = ME->CK_SEM_RET_GRUPO.
  ENDMETHOD.


  method GET_CK_SEM_RET_PEDIDO.
    R_CK_SEM_RET_PEDIDO = ME->CK_SEM_RET_PEDIDO.
  endmethod.


  METHOD GET_DS_DEPARTAMENTO.
    R_DS_DEPARTAMENTO = ME->DS_DEPARTAMENTO.
  ENDMETHOD.


  METHOD GET_GRUPO_MERCADORIA.
    R_GRUPOS = ME->GRUPO_MERCADORIA.
  ENDMETHOD.


  method GET_TIPO_PEDIDO_COMPRA.
    R_TIPOS = ME->TIPO_PEDIDO_COMPRA.
  endmethod.


  method SET_ARMAZENS.
    IF ME->ARMAZENS <> I_ARMAZENS.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->ARMAZENS = I_ARMAZENS.
  endmethod.


  METHOD SET_CD_DEPARTAMENTO.

    SELECT SINGLE * INTO @DATA(WA_ZMMT0072) FROM ZMMT0072 WHERE CD_DEPARTAMENTO = @I_CD_DEPARTAMENTO.
    IF SY-SUBRC IS INITIAL.
      MESSAGE S004 WITH I_CD_DEPARTAMENTO DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      ME->CD_DEPARTAMENTO = I_CD_DEPARTAMENTO.
    ENDIF.

  ENDMETHOD.


  method SET_CFOPS.
    IF ME->CFOPS <> I_CFOPS.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->CFOPS = I_CFOPS.
  endmethod.


  METHOD SET_CFOPS_RETORNO.
    IF ME->CFOPS_RETORNO <> I_CFOPS.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->CFOPS_RETORNO = I_CFOPS.
  ENDMETHOD.


  METHOD SET_CK_SEM_RET_GRUPO.

    IF ME->CK_SEM_RET_GRUPO NE I_CK_SEM_RET_GRUPO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->CK_SEM_RET_GRUPO = I_CK_SEM_RET_GRUPO.

  ENDMETHOD.


  METHOD SET_CK_SEM_RET_PEDIDO.

    IF ME->CK_SEM_RET_PEDIDO NE I_CK_SEM_RET_PEDIDO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->CK_SEM_RET_PEDIDO = I_CK_SEM_RET_PEDIDO.

  ENDMETHOD.


  METHOD SET_DS_DEPARTAMENTO.
    IF ME->DS_DEPARTAMENTO NE I_DS_DEPARTAMENTO.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->DS_DEPARTAMENTO = I_DS_DEPARTAMENTO.
  ENDMETHOD.


  METHOD SET_GRUPO_MERCADORIA.
    IF ME->GRUPO_MERCADORIA <> I_GRUPOS.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->GRUPO_MERCADORIA = I_GRUPOS.
  ENDMETHOD.


  method SET_TIPO_PEDIDO_COMPRA.
    IF ME->TIPO_PEDIDO_COMPRA <> I_TIPOS.
      ME->CK_ALTEROU = ABAP_TRUE.
    ENDIF.
    ME->TIPO_PEDIDO_COMPRA = I_TIPOS.
  endmethod.


  METHOD zif_cadastro~excluir_registro.

    i_excluiu = abap_false.

    IF me->validar_exclusao( ) EQ abap_true.

      DELETE FROM zmmt0073 WHERE cd_departamento EQ me->cd_departamento.
      DELETE FROM zmmt0072 WHERE cd_departamento EQ me->cd_departamento.
      DELETE FROM zmmt0075 WHERE cd_departamento EQ me->cd_departamento.
      DELETE FROM zmmt0076 WHERE cd_departamento EQ me->cd_departamento.
      DELETE FROM zmmt0077 WHERE cd_departamento EQ me->cd_departamento.
      DELETE FROM zmmt0210 WHERE cd_departamento EQ me->cd_departamento.  "*-CS2025000249-17.04.2025-#173311-JT
      DELETE FROM zmmt0211 WHERE cd_departamento EQ me->cd_departamento.  "*-CS2025000249-17.04.2025-#173311-JT
      DELETE FROM zmmt0213 WHERE cd_departamento EQ me->cd_departamento.  "*-CS2025000249-17.04.2025-#173311-JT
      COMMIT WORK.

      i_excluiu = abap_true.
      MESSAGE s007.

    ENDIF.

  ENDMETHOD.


  method ZIF_CADASTRO~GET_REGISTRO.

    DATA: WA_REGISTRO TYPE ZMMT0072.

    WA_REGISTRO-CD_DEPARTAMENTO   = ME->CD_DEPARTAMENTO.
    WA_REGISTRO-DS_DEPARTAMENTO   = ME->DS_DEPARTAMENTO.
    WA_REGISTRO-CK_SEM_RET_GRUPO  = ME->CK_SEM_RET_GRUPO.
    WA_REGISTRO-CK_SEM_RET_PEDIDO = ME->CK_SEM_RET_PEDIDO.
    MOVE-CORRESPONDING WA_REGISTRO TO E_REGISTRO.

  endmethod.


  METHOD zif_cadastro~gravar_registro.


    DATA: wa_zmmt0072 TYPE zmmt0072.

    i_gravou = abap_false.

    IF me->ck_alterou EQ abap_true.

      IF me->validar_registro( ) EQ abap_true.

*-CS2025000249-17.04.2025-#173311-JT-inicio
        IF me->ck_gravar_zmmt0210 = abap_true.
          IF me->ck_acao_gravar = 'I'.
            MODIFY zmmt0210 FROM TABLE me->empresa_tipo_pedido.
          ELSE.
            DELETE zmmt0210 FROM TABLE me->empresa_tipo_pedido.
          ENDIF.
          COMMIT WORK.
          me->ck_alterou = abap_false.
          i_gravou = abap_true.
          MESSAGE s002.
          RETURN.
        ENDIF.

        IF me->ck_gravar_zmmt0211 = abap_true.
          IF me->ck_acao_gravar = 'I'.
            MODIFY zmmt0211 FROM TABLE me->cfop_miro.
          ELSE.
            DELETE zmmt0211 FROM TABLE me->cfop_miro.
          ENDIF.
          COMMIT WORK.
          me->ck_alterou = abap_false.
          i_gravou = abap_true.
          MESSAGE s002.
          RETURN.
        ENDIF.

        IF me->ck_gravar_zmmt0213 = abap_true.
          IF me->ck_acao_gravar = 'I'.
            MODIFY zmmt0213 FROM TABLE me->empresa_classe.
          ELSE.
            DELETE zmmt0213 FROM TABLE me->empresa_classe.
          ENDIF.
          COMMIT WORK.
          me->ck_alterou = abap_false.
          i_gravou = abap_true.
          MESSAGE s002.
          RETURN.
        ENDIF.
*-CS2025000249-17.04.2025-#173311-JT-fim

        wa_zmmt0072-cd_departamento   = me->cd_departamento.
        wa_zmmt0072-ds_departamento   = me->ds_departamento.
        wa_zmmt0072-ck_sem_ret_grupo  = me->ck_sem_ret_grupo.
        wa_zmmt0072-ck_sem_ret_pedido = me->ck_sem_ret_pedido.
        MODIFY zmmt0072 FROM wa_zmmt0072.

        DELETE FROM zmmt0073 WHERE cd_departamento = me->cd_departamento.
        LOOP AT me->grupo_mercadoria INTO DATA(wa_grupo).
          wa_grupo-cd_departamento = me->cd_departamento.
          MODIFY zmmt0073 FROM wa_grupo.
        ENDLOOP.

        DELETE FROM zmmt0075 WHERE cd_departamento = me->cd_departamento.
        LOOP AT me->tipo_pedido_compra INTO DATA(wa_tipo).
          wa_tipo-cd_departamento = me->cd_departamento.
          MODIFY zmmt0075 FROM wa_tipo.
        ENDLOOP.

        DELETE FROM zmmt0076 WHERE cd_departamento = me->cd_departamento.
        LOOP AT me->armazens INTO DATA(wa_amazem).
          wa_amazem-cd_departamento = me->cd_departamento.
          MODIFY zmmt0076 FROM wa_amazem.
        ENDLOOP.

        DELETE FROM zmmt0077 WHERE cd_departamento = me->cd_departamento.
        LOOP AT me->cfops INTO DATA(wa_cfop).
          wa_cfop-cd_departamento = me->cd_departamento.
          MODIFY zmmt0077 FROM wa_cfop.
        ENDLOOP.

        DELETE FROM zmmt0119 WHERE cd_departamento = me->cd_departamento.
        LOOP AT me->cfops_retorno INTO DATA(wa_cfop_retorno).
          wa_cfop_retorno-cd_departamento = me->cd_departamento.
          MODIFY zmmt0119 FROM wa_cfop_retorno.
        ENDLOOP.

        COMMIT WORK.
        me->ck_alterou = abap_false.
        i_gravou = abap_true.
        MESSAGE s002.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD zif_cadastro~limpar_registro.

    CLEAR: me->cd_departamento,
           me->ds_departamento,
           me->ck_alterou,
           me->grupo_mercadoria,
           me->tipo_pedido_compra,
           me->armazens,
           me->cfops,
           me->cfops_retorno,
           me->empresa_tipo_pedido[],  "*-CS2025000249-17.04.2025-#173311-JT-inicio
           me->cfop_miro[],            "*-CS2025000249-17.04.2025-#173311-JT-inicio
           me->ck_gravar_zmmt0210,     "*-CS2025000249-17.04.2025-#173311-JT-inicio
           me->ck_gravar_zmmt0211,     "*-CS2025000249-17.04.2025-#173311-JT-inicio
           me->ck_gravar_zmmt0213,     "*-CS2025000249-17.04.2025-#173311-JT-inicio
           me->ck_acao_gravar.         "*-CS2025000249-17.04.2025-#173311-JT-inicio

  ENDMETHOD.


  METHOD ZIF_CADASTRO~NOVO_REGISTRO.
    ME->LIMPAR_REGISTRO( ).
  ENDMETHOD.


  METHOD zif_cadastro~set_registro.

    DATA: wa_zmmt0072 TYPE zmmt0072.

    me->limpar_registro( ).

    SELECT SINGLE * INTO wa_zmmt0072 FROM zmmt0072 WHERE cd_departamento EQ i_id_registro.

    IF sy-subrc IS INITIAL.
      me->cd_departamento = wa_zmmt0072-cd_departamento.
      me->set_ds_departamento( i_ds_departamento = wa_zmmt0072-ds_departamento ).
      me->set_ck_sem_ret_grupo( i_ck_sem_ret_grupo = wa_zmmt0072-ck_sem_ret_grupo ).
      me->set_ck_sem_ret_pedido( i_ck_sem_ret_pedido = wa_zmmt0072-ck_sem_ret_pedido ).

      SELECT * INTO TABLE @DATA(it_zmmt0073) FROM zmmt0073 WHERE cd_departamento EQ @i_id_registro.
      MOVE it_zmmt0073[] TO me->grupo_mercadoria.

      SELECT * INTO TABLE @DATA(it_zmmt0075) FROM zmmt0075 WHERE cd_departamento EQ @i_id_registro.
      MOVE it_zmmt0075[] TO me->tipo_pedido_compra.

      SELECT * INTO TABLE @DATA(it_zmmt0076) FROM zmmt0076 WHERE cd_departamento EQ @i_id_registro.
      MOVE it_zmmt0076[] TO me->armazens.

      SELECT * INTO TABLE @DATA(it_zmmt0077) FROM zmmt0077 WHERE cd_departamento EQ @i_id_registro.
      MOVE it_zmmt0077[] TO me->cfops.

      SELECT * INTO TABLE @DATA(it_zmmt0119) FROM zmmt0119 WHERE cd_departamento EQ @i_id_registro.
      MOVE it_zmmt0119[] TO me->cfops_retorno.

*-CS2025000249-17.04.2025-#173311-JT-inicio
      SELECT * INTO TABLE @DATA(it_zmmt0210) FROM zmmt0210 WHERE cd_departamento EQ @i_id_registro.
      MOVE it_zmmt0210[] TO me->empresa_tipo_pedido.

      SELECT * INTO TABLE @DATA(it_zmmt0211) FROM zmmt0211 WHERE cd_departamento EQ @i_id_registro.
      MOVE it_zmmt0211[] TO me->cfop_miro.

      SELECT * INTO TABLE @DATA(it_zmmt0213) FROM zmmt0213 WHERE cd_departamento EQ @i_id_registro.
      MOVE it_zmmt0213[] TO me->empresa_classe.
*-CS2025000249-17.04.2025-#173311-JT-fim

      me->ck_alterou = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDAR_EXCLUSAO.

    E_VALIDOU = ABAP_FALSE.

*    IF SY-TCODE NE 'VT02N' AND SY-TCODE NE 'VT01N' AND SY-TCODE NE 'ZLES0106' AND SY-TCODE NE 'ZLES0113' AND SY-TCODE NE 'ZLES0115'.
*      AUTHORITY-CHECK OBJECT 'ZREPOM' ID 'BUKRS'     FIELD ME->BUKRS
*                                      ID 'ZREPOMATV' FIELD '10'.
*      IF SY-SUBRC IS NOT INITIAL.
*        MESSAGE S063 DISPLAY LIKE 'E'.
*        EXIT.
*      ENDIF.
*    ENDIF.
*
*    CASE ME->TP_STATUS_AUT.
*      WHEN '2'.
*        MESSAGE S044 DISPLAY LIKE 'E'.
*        EXIT.
*      WHEN '3'.
*        MESSAGE S045 DISPLAY LIKE 'E'.
*        EXIT.
*    ENDCASE.
*
*    CASE ME->TP_STATUS_CAN.
*      WHEN '2'.
*        MESSAGE S046 DISPLAY LIKE 'E'.
*        EXIT.
*      WHEN '3'.
*        MESSAGE S047 DISPLAY LIKE 'E'.
*        EXIT.
*    ENDCASE.

    E_VALIDOU = ABAP_TRUE.

  ENDMETHOD.


  METHOD zif_cadastro~validar_registro.

    e_validou = abap_false.

*-CS2025000249-17.04.2025-#173311-JT-inicio
    IF me->ck_gravar_zmmt0210 = abap_true OR
       me->ck_gravar_zmmt0211 = abap_true OR
       me->ck_gravar_zmmt0213 = abap_true.
      e_validou = abap_true.
      RETURN.
    ENDIF.
*-CS2025000249-17.04.2025-#173311-JT-fim

    IF me->cd_departamento IS INITIAL.
      MESSAGE s003 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    IF me->ds_departamento IS INITIAL.
      MESSAGE s001 DISPLAY LIKE 'E'.
      EXIT.
    ENDIF.

    LOOP AT me->grupo_mercadoria INTO DATA(wa_grupos).
      SELECT SINGLE * INTO @DATA(wa_grupo)
        FROM zmmt0073 AS i
       WHERE cd_departamento NE @me->cd_departamento
         AND matkl           EQ @wa_grupos-matkl.
      IF sy-subrc IS INITIAL.
        MESSAGE s005 WITH wa_grupos-matkl wa_grupo-cd_departamento DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDLOOP.

    LOOP AT me->tipo_pedido_compra INTO DATA(wa_tipos).

      SELECT SINGLE * INTO @DATA(wa_tipo)
        FROM zmmt0075 AS i
       WHERE cd_departamento NE @me->cd_departamento
         AND bstyp           EQ @wa_tipos-bstyp
         AND bsart           EQ @wa_tipos-bsart.

      IF sy-subrc IS INITIAL.
        MESSAGE s028 WITH wa_tipos-bsart wa_tipo-cd_departamento DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDLOOP.

    e_validou = abap_true.

  ENDMETHOD.


  METHOD ZIF_CADASTRO~VALIDA_ATRIBUTO_ALTERAVEL.

    R_PERMITIDO = ABAP_FALSE.

    IF ME->CD_DEPARTAMENTO IS NOT INITIAL.
      CASE I_CAMPO.
        WHEN 'DS_DEPARTAMENTO'.
          R_PERMITIDO = ABAP_TRUE.
        WHEN 'CK_SEM_RET_GRUPO'.
          R_PERMITIDO = ABAP_TRUE.
        WHEN 'CK_SEM_RET_PEDIDO'.
          R_PERMITIDO = ABAP_TRUE.
      ENDCASE.
    ELSE.
      CASE I_CAMPO.
        WHEN 'CD_DEPARTAMENTO'.
          R_PERMITIDO = ABAP_TRUE.
        WHEN OTHERS.
          R_PERMITIDO = ABAP_TRUE.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD ZIF_PESQUISA~PESQUISAR.

    DATA: FILTRO_INTERNO TYPE ZDE_ZMMT0072_FILTRO.
    DATA: IT_ZMMT0072_R  TYPE ZDE_ZMMT0072_T.

    FILTRO_INTERNO = I_FILTROS.

    CLEAR: E_REGISTROS.

    E_PESQUISOU = ABAP_FALSE.

    SELECT * INTO TABLE @DATA(IT_ZMMT0072)
      FROM ZMMT0072
     WHERE CD_DEPARTAMENTO IN @FILTRO_INTERNO-CD_DEPARTAMENTO
       AND DS_DEPARTAMENTO IN @FILTRO_INTERNO-DS_DEPARTAMENTO.

    CHECK SY-SUBRC IS INITIAL.

    E_PESQUISOU = ABAP_TRUE.

    LOOP AT IT_ZMMT0072 INTO DATA(WA_ZMMT0072).
      APPEND WA_ZMMT0072 TO IT_ZMMT0072_R.
    ENDLOOP.

    E_REGISTROS = IT_ZMMT0072_R.

  ENDMETHOD.


  METHOD set_cfop_miro.

    IF me->cfop_miro <> i_cfop_miro.
      me->ck_alterou         = abap_true.
      me->ck_gravar_zmmt0211 = abap_true.
      me->ck_acao_gravar     = i_acao_gravar.
    ENDIF.
    me->cfop_miro            = i_cfop_miro.

  ENDMETHOD.


  METHOD set_empresa_classe.

    IF me->empresa_classe <> i_empresa_classe.
      me->ck_alterou         = abap_true.
      me->ck_gravar_zmmt0213 = abap_true.
      me->ck_acao_gravar     = i_acao_gravar.
    ENDIF.
    me->empresa_classe       = i_empresa_classe.

  ENDMETHOD.


  METHOD set_empresa_tipo_pedido.

    IF me->empresa_tipo_pedido <> i_empresa_tipo_pedido.
      me->ck_alterou         = abap_true.
      me->ck_gravar_zmmt0210 = abap_true.
      me->ck_acao_gravar     = i_acao_gravar.
    ENDIF.
    me->empresa_tipo_pedido  = i_empresa_tipo_pedido.

  ENDMETHOD.


  method ZIF_CADASTRO~CHECK_ENV_APROV_TAXA.
  endmethod.


  method ZIF_CADASTRO~SET_REGISTROEX.
  endmethod.
ENDCLASS.
