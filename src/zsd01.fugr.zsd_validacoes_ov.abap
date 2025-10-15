FUNCTION ZSD_VALIDACOES_OV.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_MATNR) TYPE  MATNR
*"     REFERENCE(I_WERKS) TYPE  WERKS_D OPTIONAL
*"  EXPORTING
*"     REFERENCE(E_MENSAGEM) TYPE  STRING
*"----------------------------------------------------------------------

  DATA: IT_MARA TYPE TABLE OF MARA,
        IT_MARC TYPE TABLE OF MARC,
        IT_MARD TYPE TABLE OF MARD,
        IT_MKAL TYPE TABLE OF MKAL,
        V_MTART TYPE MTART,
        V_MATNR TYPE MATNR.

  SELECT * FROM MARA INTO TABLE IT_MARA WHERE MATNR EQ I_MATNR.
  SELECT * FROM MARC INTO TABLE IT_MARC WHERE MATNR EQ I_MATNR AND WERKS EQ I_WERKS.
  SELECT * FROM MARD INTO TABLE IT_MARD WHERE MATNR EQ I_MATNR.
  SELECT * FROM MKAL INTO TABLE IT_MKAL WHERE MATNR EQ I_MATNR AND  ADATU LE SY-DATUM  AND BDATU GE SY-DATUM .

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = I_MATNR
    IMPORTING
      OUTPUT = V_MATNR.


  READ TABLE IT_MARA INTO DATA(W_MARA) WITH KEY MATNR = I_MATNR.
  IF SY-SUBRC IS INITIAL.
    IF W_MARA-XCHPF NE ABAP_TRUE.
      E_MENSAGEM = 'Material deve estar Configurado para ser controlado por lote, procure a Equipe de Custos e Estoques!'.
      EXIT.
    ENDIF.

    IF W_MARA-XGCHP <> ABAP_FALSE.
      E_MENSAGEM = 'Material não deve estar Configurado Protocolo de produção do batch, procure a Equipe de Custos e Estoques!'.
      EXIT.
    ENDIF.

  ENDIF.

  READ TABLE IT_MARC INTO DATA(W_MARC) WITH KEY MATNR = I_MATNR
                                                 WERKS = I_WERKS.
  IF SY-SUBRC IS INITIAL.
    IF W_MARC-LGPRO NE 'PR01'.
      CONCATENATE 'Material ' V_MATNR ' no centro ' I_WERKS 'deve estar Configurado com o depósito de Produção "PR01", procure a Equipe de Custos e Estoques!'
      INTO E_MENSAGEM  SEPARATED BY SPACE.
      EXIT.
    ENDIF.

    IF W_MARC-SFCPF NE 'PI01'.
      CONCATENATE 'Material ' V_MATNR ' no centro ' I_WERKS 'deve estar Configurado com o perfil de controle de Produção "PI01", procure a Equipe de Custos e Estoques!'
      INTO E_MENSAGEM  SEPARATED BY SPACE.
      EXIT.
    ENDIF.

    IF W_MARC-DISPO EQ ABAP_FALSE.
      CONCATENATE 'Material ' V_MATNR ' no centro ' I_WERKS 'deve estar Configurado com o planejador MRP, procure a Equipe de Custos e Estoques!'
      INTO E_MENSAGEM  SEPARATED BY SPACE.
      EXIT.
    ENDIF.

    CASE I_WERKS.
      WHEN '0175'.
        IF W_MARC-STRGR NE 'Z1'.
          CONCATENATE 'Material ' V_MATNR ' no centro ' I_WERKS 'deve estar Configurado Grupo estratégias de planejamento Z2, procure a Equipe de Custos e Estoques!'
          INTO E_MENSAGEM  SEPARATED BY SPACE.
          EXIT.
        ENDIF.
      WHEN '0124'.
        IF W_MARC-STRGR NE 'Z2'.
          CONCATENATE 'Material ' V_MATNR ' no centro ' I_WERKS 'deve estar Configurado Grupo estratégias de planejamento Z2, procure a Equipe de Custos e Estoques!'
          INTO E_MENSAGEM  SEPARATED BY SPACE.
          EXIT.
        ENDIF.
      WHEN '0161'.
        IF W_MARC-STRGR NE 'Z3'.
          CONCATENATE 'Material ' V_MATNR ' no centro ' I_WERKS 'deve estar Configurado Grupo estratégias de planejamento Z3, procure a Equipe de Custos e Estoques!'
          INTO E_MENSAGEM  SEPARATED BY SPACE.
          EXIT.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

    IF W_MARC-INSMK NE ABAP_FALSE.
      CONCATENATE 'Material ' V_MATNR ' no centro ' I_WERKS 'não deve estar Configurado como Registro em estoque em controle de qualidade, procure a Equipe de Custos e Estoques!'
      INTO E_MENSAGEM  SEPARATED BY SPACE.
      EXIT.
    ENDIF.

    IF W_MARC-EPRIO NE '0001'.
      CONCATENATE 'Material ' V_MATNR ' no centro ' I_WERKS 'deve estar Configurado com o Grupo de determinação de estoque 0001, procure a Equipe de Custos e Estoques!'
      INTO E_MENSAGEM  SEPARATED BY SPACE.
      EXIT.
    ENDIF.

    IF W_MARC-VERKZ NE ABAP_TRUE.
      CONCATENATE 'Material ' V_MATNR ' no centro ' I_WERKS 'deve estar Configurado com o Código de versão, procure a Equipe de Custos e Estoques!'
      INTO E_MENSAGEM  SEPARATED BY SPACE.
      EXIT.
    ENDIF.

    IF W_MARC-FEVOR NE 'PCP'.
      CONCATENATE 'Material ' V_MATNR ' no centro ' I_WERKS 'deve estar Configurado com o Perfil de controle de produção PCP, procure a Equipe de Custos e Estoques!'
      INTO E_MENSAGEM SEPARATED BY SPACE.
      EXIT.
    ENDIF.

    IF W_MARC-MTVFP NE '02'.
      CONCATENATE 'Material ' V_MATNR ' no centro ' I_WERKS 'deve estar Configurado com o Grupo de verificação para verificação de disponibilidade "02" procure a Equipe de Custos e Estoques!'
      INTO E_MENSAGEM SEPARATED BY SPACE.
      EXIT.
    ENDIF.

  ENDIF.

  READ TABLE IT_MARD INTO DATA(W_MARD) WITH KEY MATNR = I_MATNR
                                                WERKS = I_WERKS
                                                LGORT = 'PR01'.
  IF SY-SUBRC IS NOT INITIAL.
    E_MENSAGEM = 'Material deve estar expandido para o Depósito "PR01", procure a Equipe de Custos e Estoques!'.
    EXIT.
  ENDIF.

  READ TABLE IT_MKAL INTO DATA(W_MKAL) WITH KEY MATNR = I_MATNR .
  IF SY-SUBRC IS NOT INITIAL.
    E_MENSAGEM = 'Material não possui versão de produção válida, procure a Equipe de Custos e Estoques!'.
    EXIT.
  ENDIF.



ENDFUNCTION.
