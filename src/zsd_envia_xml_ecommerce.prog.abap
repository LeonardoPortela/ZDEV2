*&---------------------------------------------------------------------*
*&  Include  ZSD_ENVIA_XML_ECOMMERCE
*&---------------------------------------------------------------------*

*  DATA(lv_task_ecomm) = 'ZSDMF_ENVIAR_XML_ECOMMERCE' && ls_acttab-docnum.
*
*  CALL FUNCTION 'ZSDMF_ENVIAR_XML_ECOMMERCE' STARTING NEW TASK lv_task_ecomm
*    EXPORTING
*      i_acttab = ls_acttab.
