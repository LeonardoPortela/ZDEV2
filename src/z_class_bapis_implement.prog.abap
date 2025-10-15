*&-------------------------------------------------------------------------------*
*& CLASS ZBAPIS IMPLEMENTATION                                                   *
*& AUTOR: ENIO JESUS                                                             *
*& 13.07.2015                                                                    *
*&-------------------------------------------------------------------------------*

* INICIAR IMPLEMENTAÇÃO DOS MÉTODOS *

*--------------------------------------------------------------------------------*
*                                                                                *
* MÉTODO PARA REALIZAR O EMPRÉSTIMO DO EQUIPAMENTO                               *
*                                                                                *
*--------------------------------------------------------------------------------*
    METHOD Z_INICIAR_PROCESSO_EMPRESTIMO.

      DATA: R_ZUTEIS TYPE REF TO ZUTEIS.
      CREATE OBJECT R_ZUTEIS.

      LOOP AT IT_SAIDA_EMPRESTIMO_EQUI INTO WA_SAIDA_EMPRESTIMO_EQUI.
        CLEAR: INDICE, IT_STATUS_BAPIS.

****************************/DETALHES DO EQUIPAMENTO\*****************************
*--------------------------------------------------------------------------------*

        Z_DETALHES_EQUIPAMENTO( EXPORTING
                                EQUIPMENT  = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR
                                IMPORTING
                                COSTCENTER = AT_COSTCENTER_ORIGEM ).

****************************/CRIAR CENTRO CUSTO DESTINO\**************************
*--------------------------------------------------------------------------------*

        R_ZUTEIS->Z_CREATE_COSTCENTER( SWERK1 = TBX_CENTRO_DESTINO
                                       SWERK2 = TBX_CENTRO_DESTINO
                                       CENTER = AT_COSTCENTER_ORIGEM ).

*****************************/CRIAR NOTA DE EMPRÉSTIMO\***************************
*--------------------------------------------------------------------------------*

        CONCATENATE TEXT-028 TBX_CENTRO_DESTINO
               INTO SHORT_TEXT SEPARATED BY SPACE.

        Z_CRIAR_NOTA_EMPRESTIMO( EQUIPMENT  = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR
                                 SHORT_TEXT = SHORT_TEXT
                                 PRIORITY   = '3'
                                 CODE_GROUP = 'F0000010'
                                 CODING     = '0070'
                                 NOTIF_TYPE = 'Z4' ).

        R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS(
                                 TXT_STATUS = TEXT-014 ).

******************************/CRIAR ORDEM DE REMONTA\****************************
*--------------------------------------------------------------------------------*

        SHORT_TEXT = TEXT-029.
        Z_CRIAR_ORDENS_MANUTENC( ORDER_TYPE   = 'ZPM5'
                                 SHORT_TEXT   = SHORT_TEXT
                                 PLANPLANT    = TBX_CENTRO_DESTINO
                                 BUS_AREA     = TBX_CENTRO_DESTINO
                                 MN_WK_CTR    = 'OFICINA'
                                 PLANT        = TBX_CENTRO_DESTINO
                                 MAINTPLANT   = TBX_CENTRO_DESTINO
                                 LOC_BUS_AREA = TBX_CENTRO_DESTINO
                                 PLANGROUP    = 'ABS'
                                 EQUIPMENT    = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR
                                 COSTCENTER   = AT_COSTCENTER_DESTINO
                                 PMACTTYPE    = 'Z03'
                                 PRIORITY     = '4'
                                 ACTIVITY     = '0010'
                                 CONTROL_KEY  = 'PM01'
                                 DESCRIPTION  = SHORT_TEXT ).

        AT_NUMERO_ORDEM_REMONTA = Z_CRIAR_ORDENS_MANUTENC_GET( ).

        R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS(
                                    TXT_STATUS = TEXT-015 ).

**************************/CRIAR ORDEM DE ABASTECIMENTO\**************************
*--------------------------------------------------------------------------------*

        IF WA_SAIDA_EMPRESTIMO_EQUI-CBX_ORD_ABAST EQ 'X'.

          SHORT_TEXT = TEXT-030.
          Z_CRIAR_ORDENS_MANUTENC( ORDER_TYPE   = 'ZPM6'
                                   SHORT_TEXT   = SHORT_TEXT
                                   PLANPLANT    = TBX_CENTRO_DESTINO
                                   BUS_AREA     = TBX_CENTRO_DESTINO
                                   MN_WK_CTR    = 'OFICINA'
                                   PLANT        = TBX_CENTRO_DESTINO
                                   MAINTPLANT   = TBX_CENTRO_DESTINO
                                   LOC_BUS_AREA = TBX_CENTRO_DESTINO
                                   PLANGROUP    = 'ABS'
                                   EQUIPMENT    = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR
                                   COSTCENTER   = AT_COSTCENTER_DESTINO
                                   PMACTTYPE    = 'Z11'
                                   PRIORITY     = '4'
                                   ACTIVITY     = '0010'
                                   CONTROL_KEY  = 'PM01'
                                   DESCRIPTION  = SHORT_TEXT ).

          AT_NUMERO_ORDEM_ABASTEC = Z_CRIAR_ORDENS_MANUTENC_GET( ).

          R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS(
                                   TXT_STATUS = TEXT-016 ).
        ENDIF.

*************************/DESINSTALAR EQUIPAMENTO DA ORIGEM\**********************
*--------------------------------------------------------------------------------*

        Z_DESINSTAL_EQUIPAMENTO( EQUIPMENT = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR ).

*************************/ENCERRAR TODAS AS ORDENS DO EQUI\***********************
*--------------------------------------------------------------------------------*

        Z_ENCERRAR_TODAS_ORDENS(  EQUIPMENT = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR
                                 STANDORDER = WA_DATA_GENERAL-STANDORDER
                                 SETTLORDER = WA_DATA_GENERAL-SETTLORDER ).

*******************/MODIFICAR EQUIPAMENTO COM DADOS DO DESTINO\********************
*---------------------------------------------------------------------------------*

        Z_SET_ID_EQUIPAMENT( EXPORTING        SWERK = TBX_CENTRO_DESTINO
                             IMPORTING ID_EQUIPMENT = ME->AT_ID_EQUIPMENT ).

        Z_MODIFICAR_EQUIPAMENTO( EQUIPMENT = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR
                                 PLANPLANT = TBX_CENTRO_DESTINO
                                  WORK_CTR = ME->AT_ID_EQUIPMENT
                                STANDORDER = ME->AT_NUMERO_ORDEM_ABASTEC
                                SETTLORDER = ME->AT_NUMERO_ORDEM_REMONTA ).

        R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS(
                                 TXT_STATUS = TEXT-017 ).

************************/MODIFICAR OS PLANOS DO EQUIPAMENTO\************************
*----------------------------------------------------------------------------------*

        Z_SELECIONAR_PLANOS( EQUIPMENT = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR ).

        IF NOT IT_SELC_PLAN IS INITIAL.
          Z_MODIFICAR_PLANOS( EQUIPMENT = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR
                                  SWERK = TBX_CENTRO_DESTINO
                                  GSBER = TBX_CENTRO_DESTINO
                                  GEWRK = ME->AT_ID_EQUIPMENT
                                  LAUFN = ME->AT_NUMERO_ORDEM_ABASTEC ).

          R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS(
                                 TXT_STATUS = TEXT-018 ).
        ENDIF.

*********************/INSERIR DADOS DO EMPRÉSTIMO EM TABELA Z\**********************
*----------------------------------------------------------------------------------*

        R_ZUTEIS->Z_INSERT_DADOS_EMPRESTIMO(
                                EQUNR = WA_SAIDA_EMPRESTIMO_EQUI-EQUNR
                                EQKTX = WA_SAIDA_EMPRESTIMO_EQUI-EQKTX
                                SWERK = WA_DATA_GENERAL-MAINTPLANT
                                IWERK = TBX_CENTRO_DESTINO
                              QT_DIAS = TBX_QT_DIAS
                                ERDAT = SY-DATUM
                                UNAME = SY-UNAME
                          NUMERO_NOTA = WA_NOTIFHEADER_EXPORT-NOTIF_NO
                          ORDEM_ABAST = ME->AT_NUMERO_ORDEM_ABASTEC
                          ORDEM_REMON = ME->AT_NUMERO_ORDEM_REMONTA ).

      ENDLOOP.
      R_ZUTEIS->Z_ATUALIZA_STATUS_BAPIS( TXT_STATUS = TEXT-019  ).

    ENDMETHOD.                    "CREATE_ZBAPIS

*--------------------------------------------------------------------------------*
*                                                                                *
* MÉTODO PARA REALIZAR A DEVOLUÇÃO DO EQUIPAMENTO                                *
*                                                                                *
*--------------------------------------------------------------------------------*
    METHOD Z_INICIAR_PROCESSO_DEVOLUCAO.

      DATA: R_ZUTEIS TYPE REF TO ZUTEIS,
            R_ATUALIZA_TELA_POS_DEVOLUCAO TYPE REF TO Z_SELECIONA_DADOS.

      CREATE OBJECT: R_ATUALIZA_TELA_POS_DEVOLUCAO,
                     R_ZUTEIS.

      LOOP AT IT_SAIDA_EQUI_RESPONSAVEL INTO WA_SAIDA_EQUI_RESPONSAVEL WHERE
        CBX_DEVOLVER = 'X'.

****************************/ENCERRAR NOTA DE EMPRÉSTIMO**************************
*--------------------------------------------------------------------------------*

        Z_CLOSE_NOTA_EMPRESTIMO( NOTIF_NO = WA_SAIDA_EQUI_RESPONSAVEL-NUMERO_NOTA
                                  REFDATE = WA_SAIDA_EQUI_RESPONSAVEL-DT_DEVOLUCAO
                                  REFTIME = WA_SAIDA_EQUI_RESPONSAVEL-HR_DEVOLUCAO ).

****************************/DETALHES DO EQUIPAMENTO\*****************************
*--------------------------------------------------------------------------------*

        Z_DETALHES_EQUIPAMENTO( EXPORTING
                                EQUIPMENT  = WA_SAIDA_EQUI_RESPONSAVEL-EQUNR
                                IMPORTING
                                COSTCENTER = AT_COSTCENTER_ORIGEM ).

****************************/CRIAR CENTRO CUSTO DESTINO\**************************
*--------------------------------------------------------------------------------*

        R_ZUTEIS->Z_CREATE_COSTCENTER( SWERK1 = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                       SWERK2 = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                       CENTER = AT_COSTCENTER_ORIGEM ).

******************************/CRIAR ORDEM DE REMONTA\****************************
*--------------------------------------------------------------------------------*

        SHORT_TEXT = TEXT-029.
        Z_CRIAR_ORDENS_MANUTENC( ORDER_TYPE   = 'ZPM5'
                                 SHORT_TEXT   = SHORT_TEXT
                                 PLANPLANT    = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                 BUS_AREA     = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                 MN_WK_CTR    = 'OFICINA'
                                 PLANT        = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                 MAINTPLANT   = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                 LOC_BUS_AREA = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                 PLANGROUP    = 'ABS'
                                 EQUIPMENT    = WA_SAIDA_EQUI_RESPONSAVEL-EQUNR
                                 COSTCENTER   = AT_COSTCENTER_DESTINO
                                 PMACTTYPE    = 'Z03'
                                 PRIORITY     = '4'
                                 ACTIVITY     = '0010'
                                 CONTROL_KEY  = 'PM01'
                                 DESCRIPTION  = SHORT_TEXT ).

        AT_NUMERO_ORDEM_REMONTA = Z_CRIAR_ORDENS_MANUTENC_GET( ).

**************************/CRIAR ORDEM DE ABASTECIMENTO\**************************
*--------------------------------------------------------------------------------*

        IF WA_DATA_GENERAL-STANDORDER IS NOT INITIAL.

          SHORT_TEXT = TEXT-030.
          Z_CRIAR_ORDENS_MANUTENC( ORDER_TYPE   = 'ZPM6'
                                   SHORT_TEXT   = SHORT_TEXT
                                   PLANPLANT    = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                   BUS_AREA     = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                   MN_WK_CTR    = 'OFICINA'
                                   PLANT        = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                   MAINTPLANT   = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                   LOC_BUS_AREA = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                   PLANGROUP    = 'ABS'
                                   EQUIPMENT    = WA_SAIDA_EQUI_RESPONSAVEL-EQUNR
                                   COSTCENTER   = AT_COSTCENTER_DESTINO
                                   PMACTTYPE    = 'Z11'
                                   PRIORITY     = '4'
                                   ACTIVITY     = '0010'
                                   CONTROL_KEY  = 'PM01'
                                   DESCRIPTION  = SHORT_TEXT ).

          AT_NUMERO_ORDEM_ABASTEC = Z_CRIAR_ORDENS_MANUTENC_GET( ).
        ENDIF.

*************************/ENCERRAR TODAS AS ORDENS DO EQUI\***********************
*--------------------------------------------------------------------------------*

        Z_ENCERRAR_TODAS_ORDENS(  EQUIPMENT = WA_SAIDA_EQUI_RESPONSAVEL-EQUNR
                                 STANDORDER = WA_DATA_GENERAL-STANDORDER
                                 SETTLORDER = WA_DATA_GENERAL-SETTLORDER ).

*******************/MODIFICAR EQUIPAMENTO COM DADOS DO DESTINO\********************
*---------------------------------------------------------------------------------*

        Z_SET_ID_EQUIPAMENT( EXPORTING SWERK       = TBX_CENTRO_DESTINO
                             IMPORTING ID_EQUIPMENT = ME->AT_ID_EQUIPMENT ).

        Z_MODIFICAR_EQUIPAMENTO(  EQUIPMENT = WA_SAIDA_EQUI_RESPONSAVEL-EQUNR
                                  PLANPLANT = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                   WORK_CTR = ME->AT_ID_EQUIPMENT
                                 STANDORDER = ME->AT_NUMERO_ORDEM_ABASTEC
                                 SETTLORDER = ME->AT_NUMERO_ORDEM_REMONTA ).

************************/MODIFICAR OS PLANOS DO EQUIPAMENTO\************************
*----------------------------------------------------------------------------------*

        Z_SELECIONAR_PLANOS( EQUIPMENT = WA_SAIDA_EQUI_EMPRESTADOS-EQUNR ).

        IF NOT IT_SELC_PLAN IS INITIAL.

          Z_MODIFICAR_PLANOS( EQUIPMENT = WA_SAIDA_EQUI_RESPONSAVEL-EQUNR
                                  SWERK = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                  GSBER = WA_SAIDA_EQUI_RESPONSAVEL-SWERK
                                  GEWRK = ME->AT_ID_EQUIPMENT
                                  LAUFN = ME->AT_NUMERO_ORDEM_ABASTEC ).
        ENDIF.

*****************/DELETAR REGISTRO DO EQPTO DA TABELA DE EMPRÉSTIMO\****************
*----------------------------------------------------------------------------------*

        R_ZUTEIS->Z_DELETE_DADOS_EMPRESTIMO(
                             EQUIPMENT = WA_SAIDA_EQUI_RESPONSAVEL-EQUNR ).

***********************/INSTALAR EQUIPAMENTO NO LOCAL ORIGEM\***********************
*----------------------------------------------------------------------------------*

        Z_INSTALAR_EQUIPAMENTO( EQUIPMENT = WA_SAIDA_EQUI_RESPONSAVEL-EQUNR
                                    SWERK = WA_SAIDA_EQUI_RESPONSAVEL-SWERK ).
      ENDLOOP.

      MESSAGE I836(SD) WITH TEXT-038 TEXT-039 DISPLAY LIKE 'S'.

    ENDMETHOD.                    "Z_INICIAR_PROCESSO_DEVOLUCAO

**********************************************************************************
*& Descrição: Criar nota de empréstimo de equipamento                           &*
*& Atributo.: WA_NOTIFHEADER, WA_NOTIFHEADER_EXPORT, IT_RETURN, WA_RETURN       &*                                                      *
*& Parâmetro: EQUIPMENT, SHORT_TEXT, PRIORITY, CODE_GROUP, CODING, NOTIF_TYPE   &*
**********************************************************************************
    METHOD Z_CRIAR_NOTA_EMPRESTIMO.

      CLEAR: IT_RETURN, WA_NOTIFHEADER, WA_RETURN.

      WA_NOTIFHEADER-EQUIPMENT  = EQUIPMENT.
      WA_NOTIFHEADER-SHORT_TEXT = SHORT_TEXT.
      WA_NOTIFHEADER-PRIORITY   = PRIORITY.
      WA_NOTIFHEADER-CODE_GROUP = CODE_GROUP.
      WA_NOTIFHEADER-CODING     = CODING.

      CALL FUNCTION 'BAPI_ALM_NOTIF_CREATE'
        EXPORTING
          NOTIF_TYPE         = NOTIF_TYPE
          NOTIFHEADER        = WA_NOTIFHEADER
          TASK_DETERMINATION = ' '
        IMPORTING
          NOTIFHEADER_EXPORT = WA_NOTIFHEADER_EXPORT
        TABLES
          RETURN             = IT_RETURN.

      CALL FUNCTION 'BAPI_ALM_NOTIF_SAVE'
        EXPORTING
          NUMBER      = WA_NOTIFHEADER_EXPORT-NOTIF_NO
        IMPORTING
          NOTIFHEADER = WA_NOTIFHEADER_EXPORT
        TABLES
          RETURN      = IT_RETURN.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        IMPORTING
          RETURN = WA_RETURN.
    ENDMETHOD.                    "Z_NOTIF_CREATE

    METHOD: Z_CLOSE_NOTA_EMPRESTIMO.

      WA_SYSSTAT-LANGU    = SY-LANGU.
      WA_SYSSTAT-LANGUISO = SY-LANGU.
      WA_SYSSTAT-REFDATE  = REFDATE.
      WA_SYSSTAT-REFTIME  = REFTIME.

      CALL FUNCTION 'BAPI_ALM_NOTIF_CLOSE'
        EXPORTING
          NUMBER   = NOTIF_NO
          SYSTSTAT = WA_SYSSTAT
        TABLES
          RETURN   = IT_RETURN.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        IMPORTING
          RETURN = WA_RETURN.
    ENDMETHOD.                    "Z_CLOSE_NOTA_EMPRESTIMO

**********************************************************************************
*& Descrição: Criar novas ordens de manutenção com centro do destino            &*
*& Atributo.: WA_HEADER, WA_OPERATION, IT_HEADER, IT_OPERATION, IT_RETURN,      &*
*&            WA_RETURN,                                                        &*                                                      *
*& Parâmetro: EQUIPMENT, ORDER_TYPE, SHORT_TEXT, PLANPLANT, BUS_AREA, PLANT     &*
*&            MN_WK_CTR, LOC_BUS_AREA, PLANGROUP, COSTCENTER, PRIORITY          &*
*&            ACTIVITY, CONTROL_KEY, DESCRIPTION, AT_NUMERO_ORDEM               &*
**********************************************************************************
    METHOD Z_CRIAR_ORDENS_MANUTENC.

      CLEAR: IT_METHODS, IT_HEADER, IT_OPERATION,
             IT_RETURN, WA_RETURN.

      DATA: LV_ORDERID TYPE AUFNR,
            LV_REFNUM  TYPE IFREFNUM,
            LV_OPER_NO TYPE OBJIDEXT,
            NUMERO_ORDEM_V2 TYPE CHAR10.

      DEFINE ADD_WA_METHODS.
        WA_METHODS-REFNUMBER  = &1.
        WA_METHODS-OBJECTTYPE = &2.
        WA_METHODS-METHOD     = &3.
        WA_METHODS-OBJECTKEY  = &4.
        APPEND WA_METHODS TO IT_METHODS.
        CLEAR WA_METHODS.
      END-OF-DEFINITION.

      LV_ORDERID = 1.
      LV_REFNUM  = 1.

      SHIFT LV_ORDERID RIGHT DELETING TRAILING SPACE.
      TRANSLATE LV_ORDERID USING ' 0'.
      LV_ORDERID+0(1) = '%'.

      SHIFT LV_REFNUM RIGHT DELETING TRAILING SPACE.
      TRANSLATE LV_REFNUM USING ' 0'.

      LV_OPER_NO = LV_ORDERID.
      LV_OPER_NO+12(4) = '0010'.

      ADD_WA_METHODS:
        LV_REFNUM    'HEADER'  'CREATE' LV_ORDERID,
        LV_REFNUM 'OPERATION'  'CREATE' LV_OPER_NO,
        LV_REFNUM    'HEADER' 'RELEASE' LV_ORDERID,
               ''          ''    'SAVE' LV_ORDERID.

      WA_HEADER-ORDERID      = LV_ORDERID.
      WA_HEADER-ORDER_TYPE   = ORDER_TYPE.
      WA_HEADER-SHORT_TEXT   = SHORT_TEXT.
      WA_HEADER-PLANPLANT    = PLANPLANT.
      WA_HEADER-BUS_AREA     = BUS_AREA.
      WA_HEADER-MN_WK_CTR    = MN_WK_CTR.
      WA_HEADER-PLANT        = PLANT.
      WA_HEADER-MAINTPLANT   = MAINTPLANT.
      WA_HEADER-LOC_BUS_AREA = LOC_BUS_AREA.
      WA_HEADER-PLANGROUP    = PLANGROUP.
      WA_HEADER-EQUIPMENT    = EQUIPMENT.
      WA_HEADER-COSTCENTER   = COSTCENTER.
      WA_HEADER-START_DATE   = SY-DATUM.
      WA_HEADER-PRIORITY     = PRIORITY.
      APPEND WA_HEADER TO IT_HEADER.
      CLEAR WA_HEADER.

      WA_OPERATION-ACTIVITY    = ACTIVITY.
      WA_OPERATION-CONTROL_KEY = CONTROL_KEY.
      WA_OPERATION-DESCRIPTION = DESCRIPTION.
      APPEND WA_OPERATION TO IT_OPERATION.
      CLEAR WA_OPERATION.

      CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
        TABLES
          IT_METHODS   = IT_METHODS
          IT_HEADER    = IT_HEADER
          IT_OPERATION = IT_OPERATION
          RETURN       = IT_RETURN.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        IMPORTING
          RETURN = WA_RETURN.

      READ TABLE IT_RETURN INTO WA_RETURN WITH KEY NUMBER = '112'.

      ME->AT_NUMERO_ORDEM = WA_RETURN-MESSAGE_V2.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = ME->AT_NUMERO_ORDEM
        IMPORTING
          OUTPUT = ME->AT_NUMERO_ORDEM.
    ENDMETHOD.                    "Z_ORDER_MAINTAIN

**********************************************************************************
*& Descrição: Encerrar ordens de manutenção do equipamento                      &*
*& Atributo.: WA_DATA_GENERAL, WA_RETURN                                        &*
*& Parâmetro: EQUIPMENT                                                         &*
**********************************************************************************
    METHOD Z_ENCERRAR_TODAS_ORDENS.

      CLEAR: IT_METHODS, WA_RETURN.

      DATA: LV_REFNUM  TYPE IFREFNUM.

      DEFINE ADD_WA_METHODS.
        WA_METHODS-REFNUMBER  = &1.
        WA_METHODS-OBJECTTYPE = &2.
        WA_METHODS-METHOD     = &3.
        WA_METHODS-OBJECTKEY  = &4.
        APPEND WA_METHODS TO IT_METHODS.
        CLEAR WA_METHODS.
      END-OF-DEFINITION.

      LV_REFNUM  = 1.

      SHIFT LV_REFNUM RIGHT DELETING TRAILING SPACE.
      TRANSLATE LV_REFNUM USING '0'.

      IF STANDORDER     IS NOT INITIAL.
        ADD_WA_METHODS:
         LV_REFNUM 'HEADER'  'TECHNICALCOMPLETE' STANDORDER.
      ENDIF.

      IF SETTLORDER IS NOT INITIAL.
        ADD_WA_METHODS:
         LV_REFNUM 'HEADER'  'TECHNICALCOMPLETE' SETTLORDER.
      ENDIF.

      ADD_WA_METHODS:
       LV_REFNUM       ''               'SAVE' '1'.

      CALL FUNCTION 'BAPI_ALM_ORDER_MAINTAIN'
        TABLES
          IT_METHODS = IT_METHODS.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT   = 'X'
        IMPORTING
          RETURN = WA_RETURN.
    ENDMETHOD.                    "Z_ENCERRAR_TODAS_ORDENS

**********************************************************************************
*& Descrição: Recebe número da ordem criada                                     &*
*& Atributo.: AT_NUMERO_ORDEM                                                   &*
*& Retorno..: NUMERO_ORDEM                                                      &*
**********************************************************************************
    METHOD Z_CRIAR_ORDENS_MANUTENC_GET.
      NUMERO_ORDEM = ME->AT_NUMERO_ORDEM.
    ENDMETHOD.                    "Z_CRIAR_ORDENS_MANUTENC_GET

**********************************************************************************
*& Descrição: Exibir status do equipamento                                      &*
*& Parâmetro: EQUIPMENT, IT_SYSTEM_STATUS, IT_USER_STATUS                       &*
*& Atributos Globais                                                            &*
**********************************************************************************
    METHOD Z_STATUS_EQUIPAMENTO.

      CLEAR: WA_RETURN, IT_SYSTEM_STATUS,
             IT_USER_STATUS.

      CALL FUNCTION 'BAPI_EQUI_GETSTATUS'
        EXPORTING
          EQUIPMENT     = EQUIPMENT
          LANGUAGE      = SY-LANGU
        IMPORTING
          RETURN        = WA_RETURN
        TABLES
          SYSTEM_STATUS = IT_SYSTEM_STATUS
          USER_STATUS   = IT_USER_STATUS.
    ENDMETHOD.                    "Z_STATUS_EQUIPAMENTO

**********************************************************************************
*& Descrição: Exibir detalhes do equipamento                                    &*
*& Parâmetro: EQUIPMENT,                                                        &*
*& Atributos: WA_DATA_GENERAL, WA_RETUR                                         &*
**********************************************************************************
    METHOD Z_DETALHES_EQUIPAMENTO.
      CLEAR: WA_DATA_GENERAL, WA_RETURN.

      CALL FUNCTION 'BAPI_EQUI_GETDETAIL'
        EXPORTING
          EQUIPMENT        = EQUIPMENT
        IMPORTING
          DATA_GENERAL_EXP = WA_DATA_GENERAL
          RETURN           = WA_RETURN.

      AT_COSTCENTER_ORIGEM = WA_DATA_GENERAL-COSTCENTER.
    ENDMETHOD.                    "Z_MODIFICAR_EQUIPAMENTO

**********************************************************************************
*& Descrição: Desinstalar equipamento do local atual                            &*
*& Parâmetro: EQUIPMENT,                                                        &*
*& Atributos: WA_RETURN_BAPI_EQMT                                               &*
**********************************************************************************
    METHOD Z_DESINSTAL_EQUIPAMENTO.
      CLEAR: WA_RETURN_BAPI_EQMT.

      CALL FUNCTION 'BAPI_EQMT_DISMANTLEFL'
        EXPORTING
          EQUIPMENT = EQUIPMENT
          DATE      = SY-DATLO
          TIME      = SY-TIMLO
        IMPORTING
          RETURN    = WA_RETURN_BAPI_EQMT.
    ENDMETHOD.                    "Z_EQUI_DISMANTLE

**********************************************************************************
*& Descrição: Instalar equipamento no centro destino                            &*                                                                              *
*& Atributo.: AT_NUMERO_ORDEM                                                   &*
*& Parâmetro: EQUIPMENT, FUNCLOC, WA_RETURN_BAPI_EQMT                           &*
**********************************************************************************
    METHOD Z_INSTALAR_EQUIPAMENTO.
      DATA: FUNCLOC TYPE TPLNR.
      CLEAR: WA_RETURN_BAPI_EQMT.

      CONCATENATE SWERK '.FRO' INTO FUNCLOC.

      CALL FUNCTION 'BAPI_EQMT_INSTALLFL'
        EXPORTING
          EQUIPMENT = EQUIPMENT
          FUNCLOC   = FUNCLOC
          DATE      = SY-DATLO
          TIME      = SY-TIMLO
        IMPORTING
          RETURN    = WA_RETURN_BAPI_EQMT.
    ENDMETHOD.                    "Z_EQUI_INSTALLFL

**********************************************************************************
*& Descrição: Modificar equipamento com centro destino                          &*
*& Atributo.: WA_DATA_GENERAL, WA_DATA_GENERALX, WA_RETURN,                     &*
*&            WA_DATA_SPECIFIC, WA_DATA_SPECIFICX                               &*
*& Parâmetro: MAINTPLANT, PLANPLANT, BUS_AREA, COSTCENTER, WORK_CTR,            &*
*&            STANDORDER, SETTLORDER                                            &*
**********************************************************************************
    METHOD Z_MODIFICAR_EQUIPAMENTO.

      CLEAR: WA_DATA_GENERALX, WA_DATA_SPECIFIC,
             WA_DATA_SPECIFICX, WA_RETURN.

      WA_DATA_GENERAL-PLANPLANT  = PLANPLANT.
      WA_DATA_GENERAL-WORK_CTR   = WORK_CTR.
      WA_DATA_GENERAL-STANDORDER = STANDORDER.
      WA_DATA_GENERAL-SETTLORDER = SETTLORDER.

      WA_DATA_GENERALX-PLANPLANT  = 'X'.
      WA_DATA_GENERALX-WORK_CTR   = 'X'.
      WA_DATA_GENERALX-STANDORDER = 'X'.
      WA_DATA_GENERALX-SETTLORDER = 'X'.

      CALL FUNCTION 'BAPI_EQUI_CHANGE'
        EXPORTING
          EQUIPMENT      = EQUIPMENT
          DATA_GENERAL   = WA_DATA_GENERAL
          DATA_GENERALX  = WA_DATA_GENERALX
          DATA_SPECIFIC  = WA_DATA_SPECIFIC
          DATA_SPECIFICX = WA_DATA_SPECIFICX
          VALID_DATE     = SY-DATUM
          VALID_TIME     = SY-UZEIT
        IMPORTING
          RETURN         = WA_RETURN.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        EXPORTING
          WAIT   = 'X'
        IMPORTING
          RETURN = WA_RETURN.
    ENDMETHOD.                    "Z_EQUI_CHANGE

**********************************************************************************
*& Descrição: Selecionar os planos referêntes ao equipamento                    &*                                                                 &*
*& Parâmetro: EQUIPMENT                                                         &*
*& Atributos Globais                                                            &*
**********************************************************************************
    METHOD Z_SELECIONAR_PLANOS.

      DATA: IT_EQUNR TYPE RANGE OF EQUI-EQUNR,
            IT_WARPL TYPE RANGE OF VIMPLA-WARPL,
            IT_MPTYP TYPE RANGE OF VIMPLA-MPTYP,
            IT_STRAT TYPE RANGE OF VIMPLA-STRAT,
            IT_TPLNR TYPE RANGE OF VIMPLA-TPLNR,
            IT_KDAUF TYPE RANGE OF VIMPLA-KDAUF,
            IT_KDPOS TYPE RANGE OF VIMPLA-KDPOS,
            IT_PROT  TYPE RANGE OF SPROT_U,
            WA_EQUNR LIKE LINE OF IT_EQUNR.

      CLEAR: IT_EQUNR,IT_WARPL,IT_MPTYP,IT_STRAT,IT_TPLNR,
             IT_KDAUF,IT_KDPOS,IT_PROT.

*   Função "MAINTENANCE_PLAN_SELECTION" retorna os planos do eqpto.

      WA_EQUNR-LOW    = EQUIPMENT.
      WA_EQUNR-SIGN   = 'I' .
      WA_EQUNR-OPTION = 'EQ'.
      APPEND WA_EQUNR TO IT_EQUNR.
      CLEAR WA_EQUNR.

      CALL FUNCTION 'MAINTENANCE_PLAN_SELECTION'
        TABLES
          I_WARPL = IT_WARPL
          I_MPTYP = IT_MPTYP
          I_STRAT = IT_STRAT
          I_EQUNR = IT_EQUNR
          I_TPLNR = IT_TPLNR
          I_KDAUF = IT_KDAUF
          I_KDPOS = IT_KDPOS
          I_SELC  = IT_SELC_PLAN
          I_PROT  = IT_PROT.
    ENDMETHOD.                    "Z_PLAN_SELECTION

**********************************************************************************
*& Descrição: Capturar ID do equipamento. Ex: (OFICINA)                         &*
*& Parâmetro: WA_MPOS - Declarado no programa                                   &*
*& Atributos Globais                                                            &*
**********************************************************************************
    METHOD Z_SET_ID_EQUIPAMENT.
      CLEAR WA_MPOS.

      SELECT SINGLE *
        FROM MPOS
        INTO WA_MPOS
       WHERE IWERK = SWERK
         AND GEWRK <> ''.

      ME->AT_ID_EQUIPMENT = WA_MPOS-GEWRK.

    ENDMETHOD.                    "Z_SET_ID_EQUIPAMENT

**********************************************************************************
*& Descrição: Modificar todos os planos referente ao equipamento                &*
*& Atributo.: WA_IMPOS, IT_IMHIS, IT_IMPLA, IT_IMMPT, - Decl. no programa       &*
*&            WA_RETURN                                                         &*
*& Parâmetro: EQUIPMENT, IWERK, GEWRK, GSBER, LAUFN                             &*                                         *
**********************************************************************************
    METHOD Z_MODIFICAR_PLANOS.

      CLEAR: IT_IMPOS, IT_IMHIS,
             IT_IMMPT, IT_IMPLA.

*     Percorre todos os planos do equipamento

      LOOP AT IT_SELC_PLAN INTO WA_SELC_PLAN.

*     Capturar as informações do plano do equipamento.

        SELECT SINGLE *
          FROM MPOS
          INTO WA_IMPOS
         WHERE WARPL = WA_SELC_PLAN-WARPL.

*     Capturar o numerador de grupos referente ao centro destino.

        SELECT SINGLE *
          FROM PLKO
          INTO WA_PLKO
         WHERE WERKS = TBX_CENTRO_DESTINO
           AND PLNTY = 'A'.

*     Modifica o plano conforme informações do destino.

        WA_IMPOS-EQUNR = EQUIPMENT.
        WA_IMPOS-AEDAT = SY-DATUM.
        WA_IMPOS-AENAM = SY-UNAME.
        WA_IMPOS-PLNTY = WA_PLKO-PLNTY.
        WA_IMPOS-PLNNR = WA_PLKO-PLNNR.
        WA_IMPOS-PLNAL = WA_PLKO-PLNAL.
        WA_IMPOS-IWERK = SWERK.
        WA_IMPOS-GEWRK = GEWRK.
        WA_IMPOS-GSBER = GSBER.
        WA_IMPOS-LAUFN = LAUFN.
        APPEND WA_IMPOS TO IT_IMPOS.
      ENDLOOP.

      CALL FUNCTION 'MAINTENANCE_PLAN_POST'
        EXPORTING
          X_XAKTYP = 'V'
        TABLES
          IMHIS    = IT_IMHIS
          IMMPT    = IT_IMMPT
          IMPLA    = IT_IMPLA
          IMPOS    = IT_IMPOS.

      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
        IMPORTING
          RETURN = WA_RETURN.
    ENDMETHOD.                "Z_MODIFICAR_PLANOS
