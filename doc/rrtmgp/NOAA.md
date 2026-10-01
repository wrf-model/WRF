# NOAA RRTMGP 적용 코드와 WRF 이식 근거

2026년 10월 1일 기준 공식 문서와 실제 저장소를 확인했다. WRF 이식의 직접 라이브러리 공급원은 UFS가 고정한 NCAR RTE+RRTMGP이다. CCPP suite와 HAFS 설정은 인터페이스 및 향후 에어로졸·장파 산란 이식의 참고 코드로 사용한다.

## 확인한 사례

| 모델 또는 구성 | 확인 내용 | 상태와 범위 |
| --- | --- | --- |
| CCPP SCM GFS_v16_RRTMGP | CCPP v7 공식 문서가 지원 및 시험된 suite로 열거 | SCM 지원 사례이며 모든 GFS 운용 버전에 대한 근거는 아님 |
| UFS FV3 GFS_v17_p8_rrtmgp | suite, pre/cloud/aerosol/SW/LW/post 코드와 control_p8_rrtmgp 및 rad32 시험 구성 확인 | 저장소의 구현과 시험 정의 확인, 해당 UFS 시험은 실행하지 않음 |
| NOAA HAFS v2.2 | NOAA 공지가 RRTMG에서 RRTMGP 전환을 명시, HAFS 구성과 suite에 RRTMGP 코드 존재 | 시행 예정일 2026년 10월 13일, 조사일에는 미래 |
| NOAA GFDL AM5 | 2025 GFDL 검토 자료에 RTE RRTMGP 및 GFDL Cloud Optics | 개발 모델 사례, 기존 AM4.0 전체의 전환을 의미하지 않음 |
| NOAA JTTI FY25 UFS 과제 | RRTMGP 정확도·효율 개선 및 UFS 전환 과제 | 연구·전환 활동의 근거 |

CCPP 자료의 HRRR_gf와 WoFS 등 RRTMG 항목을 RRTMGP 도입 사례로 계산하지 않았다.

## 공식 문서

- [CCPP v7 Overview와 suite 목록](https://ccpp-techdoc.readthedocs.io/en/latest/Overview.html)
- [DTC CCPP SCM v7 발표](https://dtcenter.org/software-tools/common-community-physics-package-ccpp/ccpp-scm-version-7-0-0)
- [NOAA HAFS v2.2 시행 공지 SCN26 76](https://www.weather.gov/media/notification/pdf_2026/scn26-76_HAFSv2.2.pdf)
- [GFDL 2025 검토 자료 Q1 9](https://www.gfdl.noaa.gov/wp-content/uploads/2025/01/2025ReviewQ1-9_CoupledClimateEarthSystemModelsV2.pdf)
- [NOAA JTTI FY25 Awards](https://wpo.noaa.gov/jtti-program-fy25-awards/)

## 재현 가능한 코드 기준

UFS develop에서 확인한 상위 커밋은 `cd0c04c54dd879851bd2ec08e1d9113cf6b4f8ac`이다. UFSATM 하위모듈은 `6f461419f091c109d18b826ab88595da00ab336c`, CCPP physics는 `3e6660c6df54e95a0871e990c2294dd397ae3860`, RTE RRTMGP는 `41c5fcd950fed09b8afe186dede266824eca7fd3`이다.

- [UFS 시험 설정 control_p8_rrtmgp](https://github.com/ufs-community/ufs-weather-model/blob/cd0c04c54dd879851bd2ec08e1d9113cf6b4f8ac/tests/tests/control_p8_rrtmgp)
- [UFSATM CCPP 하위모듈 지정](https://github.com/NOAA-EMC/ufsatm/tree/6f461419f091c109d18b826ab88595da00ab336c/ccpp)
- [UFS CCPP Radiation RRTMGP 소스](https://github.com/ufs-community/ccpp-physics/tree/3e6660c6df54e95a0871e990c2294dd397ae3860/physics/Radiation/RRTMGP)
- [이식한 NCAR 라이브러리 커밋](https://github.com/NCAR/rte-rrtmgp/tree/41c5fcd950fed09b8afe186dede266824eca7fd3)

HAFS develop 상위 커밋 `082c1861c50718e2f9d9384a17009be3fc798301`에서 forecast UFS는 `ca72d7c4d516e5e1e48e817186aacdff57936515`, UFSATM은 `90bbdcc7625b2252f00edb28257bfab0567d16b7`, HAFS CCPP는 `e9b5fec7ac065c7b2cd910b4699da8794e5db4ef`, 라이브러리는 `763cc15f7a6d2d4f4893f83460cdd81209b6fce7`이다. HAFS는 별도의 hafs-community CCPP 저장소를 지정하므로 UFS develop과 같은 커밋으로 취급하지 않는다.

- [HAFS 구성](https://github.com/hafs-community/HAFS/blob/082c1861c50718e2f9d9384a17009be3fc798301/parm/hafs.conf)
- [HAFS regional input namelist](https://github.com/hafs-community/HAFS/blob/082c1861c50718e2f9d9384a17009be3fc798301/parm/forecast/regional/input.nml.tmp)
- [HAFS CCPP 복사 코드](https://github.com/hafs-community/ccpp-physics/tree/e9b5fec7ac065c7b2cd910b4699da8794e5db4ef/physics/Radiation/RRTMGP)

HAFS 설정의 기체 수는 6, LW/SW 밴드 수는 16/14, g점 수는 78/75이며 장파 산란을 켠다. 이번 WRF 기본 구현은 공개 g128/g112 계수로 먼저 CPU 실행 경로를 연결했다. CCPP 메타데이터와 FV3 전후처리를 그대로 옮기면 WRF 배열 및 상태 계약과 충돌하므로 WRF에 별도 어댑터를 작성했다. 이것은 이식 설계 판단이며 NOAA 운용 성능에 대한 검증 결과는 아니다.

상위 작업 디렉터리의 `research/reference_inventory.json`과 `research/upstream/`에 URL, 커밋 및 수집한 파일의 해시를 보존했다.
