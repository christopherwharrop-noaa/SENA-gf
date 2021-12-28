list( APPEND gf_kernel_common_files
machine.F90
mt19937.F90
cu_gf_driver_pre.F90
cu_gf_driver_post.F90
gf_utils.F90
cu_gf_deep.F90
cu_gf_sh.F90
cu_gf_driver.F90
gf_test.F90
)

add_executable( gf_kernel_cpu ${gf_kernel_common_files} )

if(ENABLE_GPU)
  add_executable( gf_kernel_gpu ${gf_kernel_common_files} )
  target_compile_options(gf_kernel_gpu PUBLIC ${OpenACC_Fortran_OPTIONS}:cc${CUDA_ARCH_LIST},cuda${CUDAToolkit_VERSION_MAJOR}.${CUDAToolkit_VERSION_MINOR})
endif()

if(OpenMP_FOUND)
    target_link_libraries(gf_kernel_cpu PUBLIC OpenMP::OpenMP_Fortran)
    if(ENABLE_GPU)
      target_link_libraries(gf_kernel_gpu PUBLIC OpenMP::OpenMP_Fortran)
      target_link_options(gf_kernel_gpu PUBLIC ${OpenACC_Fortran_OPTIONS}:cc${CUDA_ARCH_LIST})
    endif()
endif()

install(TARGETS gf_kernel_cpu
        DESTINATION ${PROJECT_SOURCE_DIR}/exe)
if(ENABLE_GPU)
 install(TARGETS gf_kernel_gpu
	 DESTINATION ${PROJECT_SOURCE_DIR}/exe)
endif()
