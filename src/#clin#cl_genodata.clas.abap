class /CLIN/CL_GENODATA definition
  public
  final
  create public .

public section.

  types:
    cty_t_enthd TYPE STANDARD TABLE OF /clin/entity_hd .

  class-methods GET_INSTANCE
    returning
      value(RE_O_INSTANCE) type ref to /CLIN/CL_GENODATA
    exceptions
      OBJECT_NOT_CREATED .
  methods DEFINE_DYNAMIC
    importing
      !IM_V_APP type /CLIN/APP_ID
    changing
      !CH_O_MODEL type ref to /IWBEP/IF_MGW_ODATA_MODEL .
  methods CREATE_PROPERTY
    importing
      !IM_WA_DD03L type DD03L
    changing
      !CH_O_ENTITY_TYPE type ref to /IWBEP/IF_MGW_ODATA_ENTITY_TYP .
  methods GEN_GET_ENTITY
    importing
      !IV_APP type /CLIN/APP_ID
      !IV_ENTITY_NAME type CACL_STRING optional
      !IV_ENTITY_SET_NAME type CACL_STRING optional
      !IV_SOURCE_NAME type CACL_STRING optional
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY optional
    exporting
      !ER_ENTITY type ref to DATA
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods GEN_GET_ENTITYSET
    importing
      !IV_APP type /CLIN/APP_ID
      !IV_ENTITY_NAME type STRING optional
      !IV_ENTITY_SET_NAME type STRING optional
      !IV_SOURCE_NAME type STRING optional
      !IT_FILTER_SELECT_OPTIONS type /IWBEP/T_MGW_SELECT_OPTION optional
      !IT_ORDER type /IWBEP/T_MGW_SORTING_ORDER optional
      !IS_PAGING type /IWBEP/S_MGW_PAGING optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH optional
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR optional
      !IV_FILTER_STRING type STRING optional
      !IV_SEARCH_STRING type STRING optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITYSET optional
    exporting
      !ER_ENTITYSET type ref to DATA
      !ES_RESPONSE_CONTEXT type /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  methods CREATE_DYNAMIC_TABLE
    importing
      !IM_V_APP type /CLIN/APP_ID
      !IV_ENTITY_NAME type STRING
    exporting
      !EX_V_TABNAME type TABNAME
      !EX_O_FINALLINE type ref to DATA
      !EX_O_FINALTAB1 type ref to DATA
      !EX_O_FINALTAB2 type ref to DATA .
  methods COPY_DATA_TO_REF
    importing
      !IM_IS_DATA type ANY
    changing
      !EX_CR_DATA type ref to DATA .
  methods COPY_REF_TO_DATA
    importing
      !CR_DATA type ref to DATA
    exporting
      !IS_DATA type ANY .
  methods GET_ENTHD_DATA
    importing
      !IM_V_APP type /CLIN/APP_ID
      !IM_V_ENTITY type /CLIN/ENTITY_NAME optional
    exporting
      !EX_I_ENTHD type CTY_T_ENTHD
    exceptions
      NO_DATA_FOUND .
  methods SELECT_ENTHD_DATA
    importing
      !IM_V_APP type /CLIN/APP_ID
      !IM_V_ENTITY type /CLIN/ENTITY_NAME optional
    exporting
      !EX_I_ENTHD type CTY_T_ENTHD
    exceptions
      NO_DATA_FOUND .
  methods GEN_CREATE_ENTITY
    importing
      !IV_APP type /CLIN/APP_ID
      !IV_ENTITY_NAME type STRING optional
      !IV_ENTITY_SET_NAME type STRING optional
      !IV_SOURCE_NAME type STRING optional
      !IO_DATA_PROVIDER type ref to /IWBEP/IF_MGW_ENTRY_PROVIDER
      !IT_KEY_TAB type /IWBEP/T_MGW_NAME_VALUE_PAIR optional
      !IT_NAVIGATION_PATH type /IWBEP/T_MGW_NAVIGATION_PATH optional
      !IO_TECH_REQUEST_CONTEXT type ref to /IWBEP/IF_MGW_REQ_ENTITY_C optional
    exporting
      !ER_ENTITY type ref to DATA
    raising
      /IWBEP/CX_MGW_BUSI_EXCEPTION
      /IWBEP/CX_MGW_TECH_EXCEPTION .
  PROTECTED SECTION.
private section.

  types:
    cty_t_dd03l TYPE STANDARD TABLE OF dd03l .

  class-data CA_O_INSTANCE type ref to /CLIN/CL_GENODATA .
  constants CC_FDNAME_INC type FIELDNAME value '.INC' ##NO_TEXT.
  constants CC_EXTERNAL_NAME_SET type /IWBEP/MED_EXTERNAL_NAME value 'Set' ##NO_TEXT.
  constants CC_DATATYPE_CHAR type DATATYPE_D value 'CHAR' ##NO_TEXT.
  constants CC_DATATYPE_NUMC type DATATYPE_D value 'NUMC' ##NO_TEXT.
  constants CC_DATATYPE_CLNT type DATATYPE_D value 'CLNT' ##NO_TEXT.
  constants CC_DATATYPE_DATS type DATATYPE_D value 'DATS' ##NO_TEXT.
  constants CC_DATATYPE_LANG type DATATYPE_D value 'LANG' ##NO_TEXT.
  constants CC_DATATYPE_DEC type DATATYPE_D value 'DEC' ##NO_TEXT.
  constants CC_CONV_EXIT_ISOLA type /IWBEP/IF_MGW_MED_ODATA_TYPES=>TY_E_MED_CONV_EXIT value 'ISOLA' ##NO_TEXT.
  constants CC_FILTER_STR_AND type CACL_STRING value 'AND' ##NO_TEXT.
  constants CC_STBRC type XFELD value '(' ##NO_TEXT.
  constants CC_CLBRC type XFELD value ')' ##NO_TEXT.
  constants CC_EQTO type XFELD value '=' ##NO_TEXT.
  constants CC_FDNAME_BEGDA type FDNAME value 'BEGDA' ##NO_TEXT.
  constants CC_FDNAME_ENDDA type FDNAME value 'ENDDA' ##NO_TEXT.
  constants CC_FDNAME_SUBTY type FDNAME value 'SUBTY' ##NO_TEXT.
  constants CC_INFTY_PA type INFTY value 'PA' ##NO_TEXT.
  constants CC_FDNAME_PERNR type FDNAME value 'PERNR' ##NO_TEXT.
  constants CC_SQ type XFELD value `'` ##NO_TEXT.

  methods SELECT_DD03L_DATA
    importing
      !IM_V_TABNAME type TABNAME
    exporting
      !EX_I_DD03L type CTY_T_DD03L
    exceptions
      NO_DATA_FOUND .
  methods SELECT_DYNAMIC_TABLE
    importing
      !IM_V_SINGLE type XFELD
      !IM_V_TABNAME type TABNAME
      !IM_V_FILTER_STR type STRING
      !IM_O_FINALLINE type ref to DATA
      !IM_O_FINALTAB1 type ref to DATA
      !IM_O_FINALTAB2 type ref to DATA
    exporting
      !EX_O_DATA type ref to DATA
    exceptions
      NO_DATA_FOUND .
  methods CHECK_AUTHORIZATION
    importing
      !IM_V_PERNR type PERNR_D
      !IM_V_INFTY type INFTY
      !IM_V_SUBTY type SUBTY
      !IM_V_BEGDA type BEGDA
      !IM_V_ENDDA type ENDDA
    exporting
      value(EX_V_AUTHORIZED) type BOOLEAN .
ENDCLASS.



CLASS /CLIN/CL_GENODATA IMPLEMENTATION.


  METHOD check_authorization.

    CLEAR: ex_v_authorized.

    CALL FUNCTION 'HR_CHECK_AUTHORITY_INFTY'
      EXPORTING
        pernr            = im_v_pernr
        infty            = im_v_infty
        subty            = im_v_subty
        begda            = im_v_begda
        endda            = im_v_endda
      EXCEPTIONS
        no_authorization = 1
        internal_error   = 2
        OTHERS           = 3.
    IF sy-subrc = 0.
      ex_v_authorized = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD copy_data_to_ref.

    FIELD-SYMBOLS: <l_fs_data> TYPE any.

    TRY.
        CREATE DATA ex_cr_data LIKE im_is_data.
      CATCH cx_sy_create_data_error.

        " Error handling is not required here

    ENDTRY.

    TRY.
        ASSIGN ex_cr_data->* TO <l_fs_data>.
      CATCH cx_sy_assign_cast_illegal_cast
            cx_sy_assign_cast_unknown_type
            cx_sy_assign_out_of_range.

        " Error handling is not required here

    ENDTRY.

    IF <l_fs_data> IS ASSIGNED.
      <l_fs_data> = im_is_data.
    ENDIF.

  ENDMETHOD.


  METHOD copy_ref_to_data.

    FIELD-SYMBOLS: <ls_data> TYPE any.

*    TRY.
*        CREATE DATA ex_cr_data LIKE im_is_data.
*      CATCH cx_sy_create_data_error.
*
*        " Error handling is not required here
*
*    ENDTRY.

    TRY.
        ASSIGN cr_data->* TO <ls_data>.
      CATCH cx_sy_assign_cast_illegal_cast
            cx_sy_assign_cast_unknown_type
            cx_sy_assign_out_of_range.

        " Error handling is not required here

    ENDTRY.

    IF <ls_data> IS ASSIGNED.
      is_data = <ls_data>.
    ENDIF.

  ENDMETHOD.


  METHOD create_dynamic_table.

    DATA: l_o_struct         TYPE REF TO cl_abap_structdescr,
          l_o_out_type       TYPE REF TO cl_abap_structdescr,
          l_o_out_table      TYPE REF TO cl_abap_tabledescr,

          l_o_struct_tmp     TYPE REF TO cl_abap_typedescr,
          l_i_components     TYPE STANDARD TABLE OF abap_componentdescr,
          l_i_components_new TYPE abap_component_tab.

    DATA l_v_entity_name TYPE /clin/entity_name.

    l_v_entity_name = iv_entity_name.

    CALL METHOD me->select_enthd_data
      EXPORTING
        im_v_app      = im_v_app
        im_v_entity   = l_v_entity_name
      IMPORTING
        ex_i_enthd    = DATA(l_i_enthd)
      EXCEPTIONS
        no_data_found = 1
        OTHERS        = 2. "#EC CI_SUBRC
* Sy-subrc check is not required here. Process need to carry on .


    READ TABLE l_i_enthd ASSIGNING FIELD-SYMBOL(<l_fs_enthd>) INDEX 1.

    IF <l_fs_enthd>  IS ASSIGNED.

      ex_v_tabname = <l_fs_enthd>-dbtabl_name.

      CALL METHOD cl_abap_structdescr=>describe_by_name
        EXPORTING
          p_name         = <l_fs_enthd>-dbtabl_name
        RECEIVING
          p_descr_ref    = l_o_struct_tmp
        EXCEPTIONS
          type_not_found = 1
          OTHERS         = 2.
      IF sy-subrc = 0 AND l_o_struct_tmp IS BOUND AND l_o_struct IS INITIAL.
        TRY.
            l_o_struct ?= l_o_struct_tmp.
          CATCH cx_sy_conversion_no_number
                cx_sy_conversion_overflow
                cx_sy_move_cast_error .
        ENDTRY.
      ELSE.
*            RAISE error_dynamic_table.
      ENDIF.

      IF l_o_struct IS BOUND.
        l_i_components  = l_o_struct->get_components( ).
      ENDIF.

      LOOP AT l_i_components ASSIGNING FIELD-SYMBOL(<l_fs_components>).
        APPEND <l_fs_components> TO l_i_components_new.
      ENDLOOP.

      TRY.
* 3. Create a New Type
          l_o_out_type = cl_abap_structdescr=>create( l_i_components_new ).
        CATCH cx_sy_struct_creation.

          " Error handling is not required here

      ENDTRY.

* Remove Fields Not Needed



      TRY.
          l_o_out_table = cl_abap_tabledescr=>create(
                            p_line_type  = l_o_out_type
                            p_table_kind = cl_abap_tabledescr=>tablekind_std
                            p_unique     = abap_false ).
        CATCH cx_sy_table_creation.

          " Error handling is not required here

      ENDTRY.

      CREATE DATA ex_o_finaltab1 TYPE HANDLE l_o_out_table.
      CREATE DATA ex_o_finaltab2 TYPE HANDLE l_o_out_table.
      CREATE DATA ex_o_finalline TYPE HANDLE l_o_out_type.


    ENDIF.

  ENDMETHOD.


  METHOD create_property.

    DATA: l_v_fieldname TYPE /iwbep/med_external_name,
          l_v_data_typ  TYPE datatype_d,
          l_v_element   TYPE cacl_string,
          l_v_length    TYPE i.

    DATA: l_o_property TYPE REF TO /iwbep/if_mgw_odata_property.

    l_v_fieldname  = im_wa_dd03l-fieldname.
*    REPLACE ALL OCCURRENCES OF `/GLB/` IN l_v_fieldname WITH 'GLB'.
    l_v_data_typ   = im_wa_dd03l-datatype.
    TRY.
        l_o_property = ch_o_entity_type->create_property( iv_property_name = l_v_fieldname iv_abap_fieldname = im_wa_dd03l-fieldname ).
      CATCH /iwbep/cx_mgw_med_exception.
    ENDTRY.

    IF l_o_property IS BOUND.

      IF NOT im_wa_dd03l-keyflag IS INITIAL.
        l_o_property->set_is_key( ).
      ENDIF.

      l_o_property->set_creatable( abap_false ).
      l_o_property->set_updatable( abap_false ).
      l_o_property->set_sortable( abap_false ).
      l_o_property->set_filterable( abap_true ).
      l_o_property->set_nullable( ).

      IF l_v_data_typ = cc_datatype_char OR l_v_data_typ = cc_datatype_numc OR l_v_data_typ = cc_datatype_clnt.

        l_o_property->set_type_edm_string( ).
        l_v_length = im_wa_dd03l-leng.
        l_o_property->set_maxlength( iv_max_length = l_v_length ). "#EC NOTEXT

      ELSEIF l_v_data_typ = cc_datatype_dats.

        l_o_property->set_type_edm_datetime( ).
        l_o_property->set_precison( iv_precision = 7 ).     "#EC NOTEXT

      ELSEIF l_v_data_typ = cc_datatype_lang.

        l_o_property->set_type_edm_string( ).
        l_v_length = im_wa_dd03l-leng.
        l_o_property->set_maxlength( iv_max_length = l_v_length ). "#EC NOTEXT
        l_o_property->set_conversion_exit( cc_conv_exit_isola ). "#EC NOTEXT

      ELSEIF l_v_data_typ = cc_datatype_dec.

        l_o_property->set_type_edm_decimal( ).
        l_v_length = im_wa_dd03l-leng.
        l_o_property->set_maxlength( iv_max_length = l_v_length ). "#EC NOTEXT

      ENDIF.


      TRY.
          l_v_element = im_wa_dd03l-rollname.

          CALL METHOD l_o_property->bind_data_element
            EXPORTING
              iv_element_name = l_v_element.

        CATCH /iwbep/cx_mgw_med_exception .

      ENDTRY.

    ENDIF.

  ENDMETHOD.


  METHOD define_dynamic.

    DATA:l_o_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ,
         l_o_entity_set  TYPE REF TO /iwbep/if_mgw_odata_entity_set.

    DATA: l_v_entity    TYPE /iwbep/med_external_name,
          l_v_entityset TYPE /iwbep/med_external_name.

    DATA: l_v_create    TYPE xfeld.

    CALL METHOD me->select_enthd_data
      EXPORTING
        im_v_app      = im_v_app
      IMPORTING
        ex_i_enthd    = DATA(l_i_enthd)
      EXCEPTIONS
        no_data_found = 1
        OTHERS        = 2. "#EC CI_SUBRC
* Sy-subrc check is not required here. Process need to carry on .

    LOOP AT l_i_enthd ASSIGNING FIELD-SYMBOL(<l_fs_enthd>).

      l_v_entity = <l_fs_enthd>-entity_name.

      TRY.
          l_o_entity_type = ch_o_model->create_entity_type( iv_entity_type_name = l_v_entity iv_def_entity_set = abap_false ).
        CATCH /iwbep/cx_mgw_med_exception.
      ENDTRY.

      CALL METHOD me->select_dd03l_data
        EXPORTING
          im_v_tabname  = <l_fs_enthd>-dbtabl_name
        IMPORTING
          ex_i_dd03l    = DATA(l_i_dd03l)
        EXCEPTIONS
          no_data_found = 1
          OTHERS        = 2. "#EC CI_SUBRC
* Sy-subrc check is not required here. Process need to carry on .
*Loop inside loop is not avoidable as we need all field data for each entry of outer loop.
      LOOP AT l_i_dd03l ASSIGNING FIELD-SYMBOL(<l_fs_dd03l>). "#EC CI_NESTED

*        CASE  <l_fs_enthd>-field_option.
*          WHEN cc_field_option_a.
            l_v_create = abap_true.
*          WHEN OTHERS.
*        ENDCASE.

        IF l_v_create = abap_true.

          IF  <l_fs_dd03l>-fieldname+0(4) <> cc_fdname_inc.

            CALL METHOD me->create_property
              EXPORTING
                im_wa_dd03l      = <l_fs_dd03l>
              CHANGING
                ch_o_entity_type = l_o_entity_type.

          ENDIF.

        ENDIF.

      ENDLOOP.

*********************************************************************
*   ENTITY SETS
*********************************************************************

      CONCATENATE l_v_entity cc_external_name_set INTO l_v_entityset.
      IF l_o_entity_type IS BOUND.
        TRY.
            l_o_entity_set = l_o_entity_type->create_entity_set( l_v_entityset ).
          CATCH /iwbep/cx_mgw_med_exception.
        ENDTRY.
      ENDIF.

      IF l_o_entity_set IS BOUND.

        l_o_entity_set->set_creatable( abap_false ).
        l_o_entity_set->set_updatable( abap_false ).
        l_o_entity_set->set_deletable( abap_false ).

        l_o_entity_set->set_pageable( abap_false ).
        l_o_entity_set->set_addressable( abap_false ).
        l_o_entity_set->set_has_ftxt_search( abap_false ).
        l_o_entity_set->set_subscribable( abap_false ).
        l_o_entity_set->set_filter_required( abap_true ).

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


METHOD gen_create_entity.

  DATA: lo_finalline TYPE REF TO data.

  DATA: lo_masterdata_bl      TYPE REF TO if_hrpa_masterdata_bl.

  FIELD-SYMBOLS: <fs_any> TYPE any.

  CALL METHOD me->create_dynamic_table
    EXPORTING
      im_v_app       = iv_app
      iv_entity_name = iv_entity_name
    IMPORTING
      ex_v_tabname   = DATA(lv_tabname)
      ex_o_finalline = lo_finalline.


  ASSIGN lo_finalline->* TO <fs_any>.

  CALL METHOD io_data_provider->read_entry_data
    IMPORTING
      es_data = <fs_any>.




** Get business logic from masterdata factory
*  CALL METHOD cl_hrpa_masterdata_factory=>get_business_logic
*    IMPORTING
*      business_logic = lo_masterdata_bl.
*
*  lo_masterdata_bl->initialize( ).
*
*  lo_adapter = get_adapter( iv_pernr = lv_pernr iv_infty = /clin/if_wd_constants=>gc_infty_0041 ).
*  lo_adapter->set_masterdata_bl( lo_masterdata_bl ).
*
*  lo_adapter->insert_tab( EXPORTING it_infotype = lt_p0041
*                       i_pernr     = lv_pernr
*               IMPORTING ov_is_ok  = lv_ok ).
*
*  IF lv_ok = abap_false.
*
*  ENDIF.
*
*
*  CALL METHOD lo_masterdata_bl->flush( no_commit = abap_false ).
*  COMMIT WORK.

ENDMETHOD.


  METHOD gen_get_entity.

    DATA: l_o_finaltab1 TYPE REF TO data,
          l_o_finaltab2 TYPE REF TO data,
          l_o_finalline TYPE REF TO data.

* Local Variable declaration
    DATA: l_v_field      TYPE cacl_string,
          l_v_pair       TYPE cacl_string,
          l_v_filter_str TYPE cacl_string.

    LOOP AT it_key_tab ASSIGNING FIELD-SYMBOL(<l_fs_keytab>).

      CONCATENATE cc_sq <l_fs_keytab>-value cc_sq INTO l_v_field.

      CONCATENATE cc_stbrc <l_fs_keytab>-name cc_eqto l_v_field cc_clbrc INTO l_v_pair SEPARATED BY space.

      IF l_v_filter_str IS INITIAL.
        CONCATENATE l_v_filter_str l_v_pair INTO l_v_filter_str SEPARATED BY space.
      ELSE.
        CONCATENATE l_v_filter_str cc_filter_str_and l_v_pair INTO l_v_filter_str SEPARATED BY space.
      ENDIF.

      CLEAR: l_v_field, l_v_pair.

    ENDLOOP.

    CONCATENATE cc_stbrc l_v_filter_str cc_clbrc INTO l_v_filter_str SEPARATED BY space.


    CALL METHOD me->create_dynamic_table
      EXPORTING
        im_v_app       = iv_app
        iv_entity_name = iv_entity_name
      IMPORTING
        ex_v_tabname   = DATA(l_v_tabname)
        ex_o_finalline = l_o_finalline
        ex_o_finaltab1 = l_o_finaltab1
        ex_o_finaltab2 = l_o_finaltab2.

    CALL METHOD me->select_dynamic_table
      EXPORTING
        im_v_single     = abap_true
        im_v_tabname    = l_v_tabname
        im_v_filter_str = l_v_filter_str
        im_o_finalline  = l_o_finalline
        im_o_finaltab1  = l_o_finaltab1
        im_o_finaltab2  = l_o_finaltab2
      IMPORTING
        ex_o_data       = er_entity
      EXCEPTIONS
        no_data_found   = 1
        OTHERS          = 2. "#EC CI_SUBRC
* Sy-subrc check is not required here. Process need to carry on .





  ENDMETHOD.


  METHOD gen_get_entityset.

    DATA: l_o_finaltab1 TYPE REF TO data,
          l_o_finaltab2 TYPE REF TO data,
          l_o_finalline TYPE REF TO data.

    FIELD-SYMBOLS: <l_fs_finalline> TYPE any,
                   <l_fs_finaltab>  TYPE ANY TABLE.

    DATA: l_o_filter  TYPE  REF TO /iwbep/if_mgw_req_filter.

* Local Variable declaration
    DATA: l_v_filter_str       TYPE cacl_string.

* Get Filter object
    IF io_tech_request_context IS BOUND.
      l_o_filter = io_tech_request_context->get_filter( ).
    ENDIF.

    IF l_o_filter  IS BOUND.
      l_v_filter_str    = l_o_filter->get_filter_string( ).
    ENDIF.


    CALL METHOD me->create_dynamic_table
      EXPORTING
        im_v_app       = iv_app
        iv_entity_name = iv_entity_name
      IMPORTING
        ex_v_tabname   = DATA(l_v_tabname)
        ex_o_finalline = l_o_finalline
        ex_o_finaltab1 = l_o_finaltab1
        ex_o_finaltab2 = l_o_finaltab2.

    CALL METHOD me->select_dynamic_table
      EXPORTING
        im_v_single     = abap_false
        im_v_tabname    = l_v_tabname
        im_v_filter_str = l_v_filter_str
        im_o_finalline  = l_o_finalline
        im_o_finaltab1  = l_o_finaltab1
        im_o_finaltab2  = l_o_finaltab2
      IMPORTING
        ex_o_data       = er_entityset
      EXCEPTIONS
        no_data_found   = 1
        OTHERS          = 2. "#EC CI_SUBRC
* Sy-subrc check is not required here. Process need to carry on .


  ENDMETHOD.


  METHOD get_enthd_data.

    CALL METHOD me->select_enthd_data
      EXPORTING
        im_v_app      = im_v_app
        im_v_entity   = im_v_entity
      IMPORTING
        ex_i_enthd    = ex_i_enthd
      EXCEPTIONS
        no_data_found = 1
        OTHERS        = 2.
    IF sy-subrc = 0.
      RAISE no_data_found.
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

*/ check if instance is already created
    IF ca_o_instance IS BOUND.
      re_o_instance = ca_o_instance.
    ELSE.
      TRY.
          CREATE OBJECT ca_o_instance.
          re_o_instance = ca_o_instance.
        CATCH cx_sy_create_object_error
               cx_root.
          RAISE object_not_created.
      ENDTRY.
    ENDIF.

  ENDMETHOD.


  METHOD select_dd03l_data.

* Select * is required to build dynamic structure form the contents
    SELECT * FROM dd03l INTO TABLE ex_i_dd03l WHERE tabname = im_v_tabname ORDER BY position. "#EC CI_SELECT "#EC CI_SEL_NESTED "#EC CI_SROFC_NESTED "#EC CI_EXEC_SQL_NESTED
    IF sy-subrc <> 0.
      RAISE no_data_found.
    ENDIF.

  ENDMETHOD.


  METHOD select_dynamic_table.

    FIELD-SYMBOLS: <l_fs_finalline> TYPE any,
                   <l_fs_finaltab1> TYPE ANY TABLE,
                   <l_fs_finaltab2> TYPE ANY TABLE.

    FIELD-SYMBOLS: <l_fs_pernr> TYPE any,
                   <l_fs_subty> TYPE any,
                   <l_fs_begda> TYPE any,
                   <l_fs_endda> TYPE any.

    DATA: l_v_pernr TYPE pernr_d,
          l_v_infty TYPE infty,
          l_v_subty TYPE subty,
          l_v_begda TYPE begda,
          l_v_endda TYPE endda.

* Create table and structure
    TRY.
        ASSIGN im_o_finaltab1->* TO <l_fs_finaltab1>.
        ASSIGN im_o_finaltab2->* TO <l_fs_finaltab2>.
        ASSIGN im_o_finalline->* TO <l_fs_finalline>.
      CATCH cx_sy_assign_cast_illegal_cast
            cx_sy_assign_cast_unknown_type
            cx_sy_assign_out_of_range.
        " Error handling is not required here
    ENDTRY.

    IF NOT im_v_tabname IS INITIAL.
      IF NOT im_v_single IS INITIAL.
        IF im_v_filter_str IS INITIAL.
* Select * and into corresponding fields are required as this is all dynamic
          SELECT * FROM (im_v_tabname) UP TO 1 ROWS INTO CORRESPONDING FIELDS OF <l_fs_finalline> WHERE (im_v_filter_str). "#EC CI_SELECT "#EC CI_SELCT_CORRES
          ENDSELECT.

          IF sy-subrc = 0 AND im_v_tabname+0(2) = cc_infty_pa.
            TRY.
                ASSIGN COMPONENT cc_fdname_pernr OF STRUCTURE <l_fs_finalline> TO <l_fs_pernr>.
                IF sy-subrc = 0.
                  ASSIGN COMPONENT cc_fdname_subty OF STRUCTURE <l_fs_finalline> TO <l_fs_subty>.
                  IF sy-subrc = 0.
                    ASSIGN COMPONENT cc_fdname_begda OF STRUCTURE <l_fs_finalline> TO <l_fs_begda>.
                    IF sy-subrc = 0.
                      ASSIGN COMPONENT cc_fdname_endda OF STRUCTURE <l_fs_finalline> TO <l_fs_endda>.
                      IF sy-subrc = 0.
                        l_v_infty = im_v_tabname+2(4).

                        CALL METHOD me->check_authorization
                          EXPORTING
                            im_v_pernr      = <l_fs_pernr>
                            im_v_infty      = l_v_infty
                            im_v_subty      = <l_fs_subty>
                            im_v_begda      = <l_fs_begda>
                            im_v_endda      = <l_fs_endda>
                          IMPORTING
                            ex_v_authorized = DATA(l_v_authorized).

                        IF l_v_authorized = abap_true.
*     Send specific entity data to the caller interface only authorized ones in case of PA tables
                          copy_data_to_ref(
                            EXPORTING
                              im_is_data = <l_fs_finalline>
                            CHANGING
                              ex_cr_data = ex_o_data ).
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                ENDIF.

              CATCH cx_sy_assign_cast_illegal_cast
              cx_sy_assign_cast_unknown_type
              cx_sy_assign_out_of_range.
                " Error handling is not required here
            ENDTRY.
          ELSE.
*     Send specific entity data to the caller interface
            copy_data_to_ref(
              EXPORTING
                im_is_data = <l_fs_finalline>
              CHANGING
                ex_cr_data = ex_o_data ).
          ENDIF.

        ENDIF.
      ELSE.
        IF NOT im_v_filter_str IS INITIAL.
* Select * and into corresponding fields are required as this is all dynamic
          SELECT * FROM (im_v_tabname) INTO CORRESPONDING FIELDS OF TABLE <l_fs_finaltab1> WHERE (im_v_filter_str). "#EC CI_SELECT "#EC CI_SELCT_CORRES

          IF sy-subrc = 0 AND im_v_tabname+0(2) = cc_infty_pa.
            LOOP AT <l_fs_finaltab1> ASSIGNING <l_fs_finalline>.
              TRY.
                  ASSIGN COMPONENT cc_fdname_pernr OF STRUCTURE <l_fs_finalline> TO <l_fs_pernr>.
                  IF sy-subrc = 0.
                    ASSIGN COMPONENT cc_fdname_subty OF STRUCTURE <l_fs_finalline> TO <l_fs_subty>.
                    IF sy-subrc = 0.
                      ASSIGN COMPONENT cc_fdname_begda OF STRUCTURE <l_fs_finalline> TO <l_fs_begda>.
                      IF sy-subrc = 0.
                        ASSIGN COMPONENT cc_fdname_endda OF STRUCTURE <l_fs_finalline> TO <l_fs_endda>.
                        IF sy-subrc = 0.
                          l_v_infty = im_v_tabname+2(4).

                          CALL METHOD me->check_authorization
                            EXPORTING
                              im_v_pernr      = <l_fs_pernr>
                              im_v_infty      = l_v_infty
                              im_v_subty      = <l_fs_subty>
                              im_v_begda      = <l_fs_begda>
                              im_v_endda      = <l_fs_endda>
                            IMPORTING
                              ex_v_authorized = l_v_authorized.

                          IF l_v_authorized = abap_true.
                            INSERT <l_fs_finalline> INTO TABLE <l_fs_finaltab2>.
                          ENDIF.
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  ENDIF.

                CATCH cx_sy_assign_cast_illegal_cast
                cx_sy_assign_cast_unknown_type
                cx_sy_assign_out_of_range.
                  " Error handling is not required here
              ENDTRY.
            ENDLOOP.
*     Send specific entity data to the caller interface only authorized ones in case of PA tables
            copy_data_to_ref(
              EXPORTING
                im_is_data = <l_fs_finaltab2>
              CHANGING
                ex_cr_data = ex_o_data ).

          ELSE.
*     Send specific entity data to the caller interface
            copy_data_to_ref(
              EXPORTING
                im_is_data = <l_fs_finaltab1>
              CHANGING
                ex_cr_data = ex_o_data ).
          ENDIF.

        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD select_enthd_data.

* Very few fields exist in this table and everything needs to be selected
    IF im_v_entity IS INITIAL.
      SELECT * FROM /clin/entity_hd INTO TABLE ex_i_enthd WHERE app_id = im_v_app. "#EC CI_SELECT
    ELSE.
      SELECT * FROM /clin/entity_hd INTO TABLE ex_i_enthd WHERE app_id = im_v_app AND entity_name = im_v_entity. "#EC CI_SELECT
    ENDIF.
    IF sy-subrc <> 0.
      RAISE no_data_found.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
