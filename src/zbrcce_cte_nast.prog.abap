*&---------------------------------------------------------------------*
*& Report  J_1BNFPR                                                    *
*&                                                                     *
*&---------------------------------------------------------------------*
*&  Print electronic fiscal document                                   *
*&  Should be used together with Message Control (NAST)                *
*&---------------------------------------------------------------------*

REPORT  ZBRCCE_CTE_NAST MESSAGE-ID 8B.

TABLES: J_1BNFDOC, NAST, TNAPR.

DATA: OUTPUT_OPTIONS     TYPE SSFCOMPOP,
      CONTROL_PARAMETERS TYPE SSFCTRLOP.


TYPES: BEGIN OF TY_PARCEIRO,
         RAZAO_SOC   TYPE LFA1-NAME1,
         CNPJ        TYPE LFA1-STCD1,
         ENDERECO    TYPE ADRC-STREET,
         BAIRRO      TYPE ADRC-CITY2,
         CEP         TYPE ADRC-POST_CODE1,
         MUNICIPIO   TYPE ADRC-CITY1,
         FONE        TYPE ADRC-FAX_NUMBER,
         UF          TYPE ADRC-REGION,
         IE          TYPE LFA1-STCD3,
       END OF TY_PARCEIRO.

DATA: GWA_CCE             TYPE ZCARTA_CORRECAO,
      GWA_J_1BNFDOC       TYPE J_1BNFDOC,
      GWA_J_1BNFE_ACTIVE  TYPE J_1BNFE_ACTIVE,
      GWA_EMITENTE        TYPE TY_PARCEIRO,
      GWA_DESTINATARIO    TYPE TY_PARCEIRO,
      GIT_ZSDT0081        TYPE TABLE OF ZSDT0081.

DATA: GVA_DOCNUM TYPE ZCARTA_CORRECAO-DOCNUM,
      GVA_ID_CC  TYPE ZCARTA_CORRECAO-ID_CC.

DATA: GWA_DADOS_CCE      TYPE ZBRCCE_CTE.

CONSTANTS: GCO_EDIT_MASK(54) TYPE C VALUE '____.____.____.____.____.____.____.____.____.____.____'.


INITIALIZATION.
  PERFORM FM_TESTE.

FORM FM_ENTRY USING RETURN_CODE US_SCREEN.

  DATA: OTFDATA TYPE TSFOTF.

  TNAPR-SFORM = 'ZBRCCE_CTE'.

  PERFORM FM_IMPRIMIR_CCE USING RETURN_CODE
                      CHANGING OTFDATA.

  CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
    EXPORTING
      I_OTF                    = OTFDATA
    EXCEPTIONS
      CONVERT_OTF_TO_PDF_ERROR = 1
      CNTL_ERROR               = 2
      OTHERS                   = 3.

ENDFORM.                               " ENTRY

FORM FM_ENTRY2 USING RETURN_CODE US_SCREEN P_NAST TYPE NAST
         CHANGING OTFDATA TYPE TSFOTF.

  MOVE-CORRESPONDING P_NAST TO NAST.

  TNAPR-SFORM = 'ZBRCCE_CTE'.

  PERFORM FM_IMPRIMIR_CCE USING RETURN_CODE CHANGING OTFDATA.

ENDFORM.                               " ENTRY

FORM FM_IMPRIMIR_CCE USING P_RETURN_CODE
                  CHANGING OTFDATA  TYPE TSFOTF.

  CLEAR P_RETURN_CODE.

  PERFORM FM_GET_DADOS_CCE.

  PERFORM FM_BUILD_INFO.

  CHECK GWA_DADOS_CCE IS NOT INITIAL.

  PERFORM FM_PRINTING CHANGING OTFDATA.

ENDFORM.

FORM FM_BUILD_INFO.

  CLEAR: GWA_DADOS_CCE.

*------------------------------------------------------------------------------------------*
* Dados Documento Eletronico
*------------------------------------------------------------------------------------------*

  GWA_DADOS_CCE-REGIO                  = GWA_J_1BNFE_ACTIVE-REGIO.
  GWA_DADOS_CCE-MODELO                 = GWA_J_1BNFE_ACTIVE-MODEL.
  GWA_DADOS_CCE-SERIE                  = GWA_J_1BNFE_ACTIVE-SERIE.
  GWA_DADOS_CCE-NUMERO                 = GWA_J_1BNFE_ACTIVE-NFNUM9.

  GWA_DADOS_CCE-PROTOCOLO_DOC          = GWA_J_1BNFE_ACTIVE-AUTHCOD.
  GWA_DADOS_CCE-DT_AUTH_DOC            = GWA_J_1BNFE_ACTIVE-AUTHDATE.
  GWA_DADOS_CCE-HR_AUTH_DOC            = GWA_J_1BNFE_ACTIVE-AUTHTIME.

  GWA_DADOS_CCE-DTEMI                  = GWA_J_1BNFDOC-DOCDAT.
  GWA_DADOS_CCE-CHAVE                  = GWA_J_1BNFE_ACTIVE-REGIO && GWA_J_1BNFE_ACTIVE-NFYEAR && GWA_J_1BNFE_ACTIVE-NFMONTH && GWA_J_1BNFE_ACTIVE-STCD1 && GWA_J_1BNFE_ACTIVE-MODEL &&
                                         GWA_J_1BNFE_ACTIVE-SERIE && GWA_J_1BNFE_ACTIVE-NFNUM9 && GWA_J_1BNFE_ACTIVE-DOCNUM9 && GWA_J_1BNFE_ACTIVE-CDV.

  WRITE GWA_DADOS_CCE-CHAVE TO GWA_DADOS_CCE-CHAVE_MASK USING EDIT MASK GCO_EDIT_MASK.

  CASE GWA_J_1BNFE_ACTIVE-TPAMB.
    WHEN '1'.
      GWA_DADOS_CCE-DS_AMBIENTE        = 'PRODUÇÃO'.
    WHEN '2'.
      GWA_DADOS_CCE-DS_AMBIENTE        = 'HOMOLOGAÇÃO'.
  ENDCASE.

*------------------------------------------------------------------------------------------*
* Dados Evento
*------------------------------------------------------------------------------------------*

  GWA_DADOS_CCE-DT_AUTH_EVENTO         = GWA_CCE-DT_AUTHCOD.
  GWA_DADOS_CCE-HR_AUTH_EVENTO         = GWA_CCE-HR_AUTHCOD.
  GWA_DADOS_CCE-SEQ_EVENTO             = GWA_CCE-ID_CC.
  GWA_DADOS_CCE-PROTOCOLO_EVENTO       = GWA_CCE-AUTHCODE.

*------------------------------------------------------------------------------------------*
* Dados Emitente
*------------------------------------------------------------------------------------------*

  GWA_DADOS_CCE-EMIT_RAZAO_SOC         = GWA_EMITENTE-RAZAO_SOC.
  GWA_DADOS_CCE-EMIT_CNPJ              = GWA_EMITENTE-CNPJ.
  GWA_DADOS_CCE-EMIT_ENDERECO          = GWA_EMITENTE-ENDERECO.
  GWA_DADOS_CCE-EMIT_BAIRRO            = GWA_EMITENTE-BAIRRO.
  GWA_DADOS_CCE-EMIT_CEP               = GWA_EMITENTE-CEP.
  GWA_DADOS_CCE-EMIT_MUNICIPIO         = GWA_EMITENTE-MUNICIPIO.
  GWA_DADOS_CCE-EMIT_FONE              = GWA_EMITENTE-FONE.
  GWA_DADOS_CCE-EMIT_UF                = GWA_EMITENTE-UF.
  GWA_DADOS_CCE-EMIT_IE                = GWA_EMITENTE-IE.

  PERFORM FM_FORMATA_CGC USING GWA_DADOS_CCE-EMIT_CNPJ
                      CHANGING GWA_DADOS_CCE-EMIT_CNPJ.

*------------------------------------------------------------------------------------------*
* Dados Destinatario
*------------------------------------------------------------------------------------------*

  GWA_DADOS_CCE-DEST_RAZAO_SOC         = GWA_DESTINATARIO-RAZAO_SOC.
  GWA_DADOS_CCE-DEST_CNPJ              = GWA_DESTINATARIO-CNPJ.
  GWA_DADOS_CCE-DEST_ENDERECO          = GWA_DESTINATARIO-ENDERECO.
  GWA_DADOS_CCE-DEST_BAIRRO            = GWA_DESTINATARIO-BAIRRO.
  GWA_DADOS_CCE-DEST_CEP               = GWA_DESTINATARIO-CEP.
  GWA_DADOS_CCE-DEST_MUNICIPIO         = GWA_DESTINATARIO-MUNICIPIO.
  GWA_DADOS_CCE-DEST_FONE              = GWA_DESTINATARIO-FONE.
  GWA_DADOS_CCE-DEST_UF                = GWA_DESTINATARIO-UF.
  GWA_DADOS_CCE-DEST_IE                = GWA_DESTINATARIO-IE.

  PERFORM FM_FORMATA_CGC USING GWA_DADOS_CCE-DEST_CNPJ
                      CHANGING GWA_DADOS_CCE-DEST_CNPJ.

*------------------------------------------------------------------------------------------*
* Dados Correção
*------------------------------------------------------------------------------------------*

  CONCATENATE  'A Carta de Correcao e disciplinada pelo Art. 58-B do CONVENIO/SINIEF 06/89: Fica'
               'permitida a utilizacao de carta de correcao, para regularizacao de erro ocorrido'
               'na emissao de documentos fiscais relativos a prestacao de servico de transporte,'
               'desde que o erro nao esteja relacionado com: I - as variaveis que determinam o'
               'valor do imposto tais como: base de calculo, aliquota, diferenca de preco,'
               'quantidade, valor da prestacao;II - a correcao de dados cadastrais'
               'que implique mudanca do emitente, tomador, remetente ou do'
               'destinatario;III - a data de emissao ou de saida.'
         INTO  GWA_DADOS_CCE-CONDICOES_USO  SEPARATED BY SPACE.

  LOOP AT GIT_ZSDT0081 INTO DATA(LWA_ZSDT0081).

   CONCATENATE LWA_ZSDT0081-VALOR
               LWA_ZSDT0081-VALOR1
               LWA_ZSDT0081-VALOR2
               LWA_ZSDT0081-VALOR3 INTO DATA(LVA_VALOR_CORRETO) SEPARATED BY SPACE.

    APPEND VALUE #( GRUPO = LWA_ZSDT0081-GRUPO CAMPO = LWA_ZSDT0081-CAMPO CONSIDERAR_CORRETO = LVA_VALOR_CORRETO ) TO GWA_DADOS_CCE-CORRECOES.
  ENDLOOP.

  SORT GWA_DADOS_CCE-CORRECOES BY GRUPO.

  CONCATENATE GWA_CCE-MSG_CORREC1
              GWA_CCE-MSG_CORREC2
              GWA_CCE-MSG_CORREC3
              GWA_CCE-MSG_CORREC4 INTO GWA_DADOS_CCE-CONSIDERAR_CORRETO.

ENDFORM.


FORM FM_TESTE.

  DATA: OTFDATA1      TYPE TSFOTF,
        VQTDE         TYPE J_1BNFLIN-MENGE,
        WL_DOC_ORIGIN TYPE ZDE_DOCS_ORIGIN_CTE.

  TNAPR-SFORM = 'ZBRCCE_CTE'.

  NAST-OBJKY =  '0008860939' && '01'.

  PERFORM FM_GET_DADOS_CCE.

  PERFORM FM_BUILD_INFO.

  PERFORM FM_PRINTING CHANGING OTFDATA1.

  CALL FUNCTION 'ZSMARTFORMS_PDF_PREVIEW'
    EXPORTING
      I_OTF                    = OTFDATA1
    EXCEPTIONS
      CONVERT_OTF_TO_PDF_ERROR = 1
      CNTL_ERROR               = 2
      OTHERS                   = 3.

ENDFORM.


FORM FM_PRINTING CHANGING OTFDATA TYPE TSFOTF.

  DATA: T_JOB_OUTPUT_INFO TYPE  SSFCRESCL,
        FM_NAME           TYPE RS38L_FNAM.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      FORMNAME           = TNAPR-SFORM
    IMPORTING
      FM_NAME            = FM_NAME
    EXCEPTIONS
      NO_FORM            = 1
      NO_FUNCTION_MODULE = 2
      OTHERS             = 3.

  OUTPUT_OPTIONS-TDNEWID  = 'X'.
  OUTPUT_OPTIONS-TDDEST   = NAST-LDEST.
  OUTPUT_OPTIONS-TDIMMED  = NAST-DIMME.                     "1897281
  OUTPUT_OPTIONS-TDDELETE = 'X'.                            "1897281

  CLEAR: CONTROL_PARAMETERS.

  CONTROL_PARAMETERS-NO_DIALOG = 'X'.
  CONTROL_PARAMETERS-DEVICE    = 'PRINTER'.
  CONTROL_PARAMETERS-PREVIEW   = ' '.
  CONTROL_PARAMETERS-GETOTF    = 'X'.


  CALL FUNCTION FM_NAME
    EXPORTING
      CONTROL_PARAMETERS = CONTROL_PARAMETERS
      OUTPUT_OPTIONS     = OUTPUT_OPTIONS
      USER_SETTINGS      = ''
      I_DADOS_CCE       = GWA_DADOS_CCE
    IMPORTING
      JOB_OUTPUT_INFO    = T_JOB_OUTPUT_INFO
    EXCEPTIONS
      FORMATTING_ERROR   = 1
      INTERNAL_ERROR     = 2
      SEND_ERROR         = 3
      USER_CANCELED      = 4
      OTHERS             = 5.
  IF SY-SUBRC = 0.
    MOVE T_JOB_OUTPUT_INFO-OTFDATA TO OTFDATA.
  ENDIF.



ENDFORM.                    "printing


FORM FM_GET_DADOS_CCE.

  DATA: LVA_PARID  TYPE J_1BPARID.

  DATA: LWA_ADRC   TYPE ADRC,
        LWA_LFA1   TYPE LFA1,
        LWA_KNA1   TYPE KNA1.

  CLEAR: GWA_CCE, GWA_DESTINATARIO, GWA_EMITENTE, GWA_J_1BNFDOC, GWA_DADOS_CCE, LVA_PARID, GIT_ZSDT0081[].

  CHECK ( NAST-OBJKY IS NOT INITIAL ) AND ( STRLEN( NAST-OBJKY ) EQ 12 ).

  MOVE: NAST-OBJKY+00(10) TO GVA_DOCNUM,
        NAST-OBJKY+10(02) TO GVA_ID_CC.

  SELECT SINGLE *
    FROM ZCARTA_CORRECAO INTO GWA_CCE
   WHERE DOCNUM EQ GVA_DOCNUM
     AND ID_CC  EQ GVA_ID_CC.

  CHECK SY-SUBRC EQ 0.

  SELECT SINGLE *
    FROM J_1BNFDOC INTO GWA_J_1BNFDOC
   WHERE DOCNUM EQ GVA_DOCNUM.

  CHECK SY-SUBRC EQ 0.

  SELECT SINGLE *
    FROM J_1BNFE_ACTIVE INTO GWA_J_1BNFE_ACTIVE
   WHERE DOCNUM EQ GVA_DOCNUM.

  CHECK SY-SUBRC EQ 0.

  SELECT *
    FROM ZSDT0081 INTO TABLE GIT_ZSDT0081
   WHERE DOCNUM EQ GVA_DOCNUM
     AND ID_CC  EQ GVA_ID_CC.

  CHECK GIT_ZSDT0081[] IS NOT INITIAL.

" Determinar Dados Emitente

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT     = GWA_J_1BNFDOC-BRANCH
    IMPORTING
      OUTPUT    = LVA_PARID.

  CALL FUNCTION 'Z_PARCEIRO_INFO'
    EXPORTING
      P_PARCEIRO         = LVA_PARID
      P_PARTYPE          = 'B'
      P_ENDERECO         = ABAP_TRUE
    CHANGING
      WA_INFO_PART       = LWA_LFA1
      WA_INFO_C          = LWA_KNA1
      WA_ADRC            = LWA_ADRC.

  IF LWA_LFA1 IS NOT INITIAL.

    GWA_EMITENTE-RAZAO_SOC    = LWA_LFA1-NAME1.
    GWA_EMITENTE-CNPJ         = LWA_LFA1-STCD1.
    GWA_EMITENTE-ENDERECO     = LWA_ADRC-STREET.
    GWA_EMITENTE-BAIRRO       = LWA_ADRC-CITY2.
    GWA_EMITENTE-CEP          = LWA_ADRC-POST_CODE1.
    GWA_EMITENTE-MUNICIPIO    = LWA_ADRC-CITY1.
    GWA_EMITENTE-FONE         = LWA_LFA1-TELF1.
    GWA_EMITENTE-UF           = LWA_ADRC-REGION.
    GWA_EMITENTE-IE           = LWA_LFA1-STCD3.

  ELSEIF LWA_KNA1 IS NOT INITIAL.

    GWA_EMITENTE-RAZAO_SOC    = LWA_KNA1-NAME1.
    GWA_EMITENTE-CNPJ         = LWA_KNA1-STCD1.
    GWA_EMITENTE-ENDERECO     = LWA_ADRC-STREET.
    GWA_EMITENTE-BAIRRO       = LWA_ADRC-CITY2.
    GWA_EMITENTE-CEP          = LWA_ADRC-POST_CODE1.
    GWA_EMITENTE-MUNICIPIO    = LWA_ADRC-CITY1.
    GWA_EMITENTE-FONE         = LWA_KNA1-TELF1.
    GWA_EMITENTE-UF           = LWA_ADRC-REGION.
    GWA_EMITENTE-IE           = LWA_KNA1-STCD3.

  ELSE.
    EXIT.
  ENDIF.



" Determinar Dados Destinatario

  CALL FUNCTION 'Z_PARCEIRO_INFO'
    EXPORTING
      P_PARCEIRO         = GWA_J_1BNFDOC-PARID
      P_PARTYPE          = GWA_J_1BNFDOC-PARTYP
      P_ENDERECO         = ABAP_TRUE
    CHANGING
      WA_INFO_PART       = LWA_LFA1
      WA_INFO_C          = LWA_KNA1
      WA_ADRC            = LWA_ADRC.


  IF LWA_LFA1 IS NOT INITIAL.

    GWA_DESTINATARIO-RAZAO_SOC    = LWA_LFA1-NAME1.
    GWA_DESTINATARIO-CNPJ         = LWA_LFA1-STCD1.
    GWA_DESTINATARIO-ENDERECO     = LWA_ADRC-STREET.
    GWA_DESTINATARIO-BAIRRO       = LWA_ADRC-CITY2.
    GWA_DESTINATARIO-CEP          = LWA_ADRC-POST_CODE1.
    GWA_DESTINATARIO-MUNICIPIO    = LWA_ADRC-CITY1.
    GWA_DESTINATARIO-FONE         = LWA_LFA1-TELF1.
    GWA_DESTINATARIO-UF           = LWA_ADRC-REGION.
    GWA_DESTINATARIO-IE           = LWA_LFA1-STCD3.

  ELSEIF LWA_KNA1 IS NOT INITIAL.

    GWA_DESTINATARIO-RAZAO_SOC    = LWA_KNA1-NAME1.
    GWA_DESTINATARIO-CNPJ         = LWA_KNA1-STCD1.
    GWA_DESTINATARIO-ENDERECO     = LWA_ADRC-STREET.
    GWA_DESTINATARIO-BAIRRO       = LWA_ADRC-CITY2.
    GWA_DESTINATARIO-CEP          = LWA_ADRC-POST_CODE1.
    GWA_DESTINATARIO-MUNICIPIO    = LWA_ADRC-CITY1.
    GWA_DESTINATARIO-FONE         = LWA_KNA1-TELF1.
    GWA_DESTINATARIO-UF           = LWA_ADRC-REGION.
    GWA_DESTINATARIO-IE           = LWA_KNA1-STCD3.

  ELSE.
    EXIT.
  ENDIF.


ENDFORM.


FORM FM_GET_DATA_HORA_UTC  USING P_DATA_HORA
                       CHANGING C_DATA
                                C_HORA.

  CHECK P_DATA_HORA IS NOT INITIAL.

  PERFORM FM_GET_DATA_UTC USING P_DATA_HORA
                      CHANGING C_DATA.

  PERFORM FM_GET_HORA_UTC USING P_DATA_HORA
                      CHANGING C_HORA.

ENDFORM.


FORM FM_GET_DATA_UTC USING P_DATA_HORA
                 CHANGING C_DATA.

  CHECK P_DATA_HORA IS NOT INITIAL.

  C_DATA = P_DATA_HORA(4) &&
           P_DATA_HORA+05(02) &&
           P_DATA_HORA+08(02).

ENDFORM.

FORM FM_GET_HORA_UTC USING P_DATA_HORA
                 CHANGING C_HORA.

  CHECK P_DATA_HORA IS NOT INITIAL.

  C_HORA  = P_DATA_HORA+11(02) &&
            P_DATA_HORA+14(02) &&
            P_DATA_HORA+17(02).

ENDFORM.

FORM FM_FORMATA_CGC USING P_CGC
                 CHANGING P_CGC_FORMAT.


  DATA LC_CGC_AUX TYPE PBR99_CGC.

  CHECK NOT P_CGC IS INITIAL.

  CHECK P_CGC > 0.

  LC_CGC_AUX = P_CGC.

  CALL FUNCTION 'HR_BR_CHECK_CGC_FORMAT'
    EXPORTING
      CGC_NUMBER               = LC_CGC_AUX
    IMPORTING
      CGC_NUMBER_FORMATTED     = LC_CGC_AUX
    EXCEPTIONS
      CGC_FORMAT_NOT_SUPPORTED = 1
      CGC_CHECK_DIGIT          = 2
      OTHERS                   = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  MOVE LC_CGC_AUX TO P_CGC_FORMAT.

ENDFORM.                    " ZF_FORMATA_CGC


*&---------------------------------------------------------------------*
*&      Form  zf_formata_cpf
*&---------------------------------------------------------------------*
* Formata CPF
*----------------------------------------------------------------------*
FORM FM_FORMATA_CPF USING    P_CPF
                    CHANGING P_CPF_FORMAT.

  DATA LC_CPF_AUX TYPE PBR99_CPF.

  CLEAR P_CPF_FORMAT.

  CHECK NOT P_CPF IS INITIAL.

  LC_CPF_AUX = P_CPF.

  CALL FUNCTION 'HR_BR_CHECK_CPF_FORMAT'
    EXPORTING
      CPF_NUMBER               = LC_CPF_AUX
    IMPORTING
      CPF_NUMBER_FORMATTED     = LC_CPF_AUX
    EXCEPTIONS
      CPF_FORMAT_NOT_SUPPORTED = 1
      CPF_CHECK_DIGIT          = 2
      OTHERS                   = 3.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  MOVE LC_CPF_AUX TO P_CPF_FORMAT.

ENDFORM.                    "zf_formata_cpf
