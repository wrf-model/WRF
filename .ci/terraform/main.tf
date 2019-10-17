provider "aws" {
   region     = "${var.region}"
   profile    = "${var.aws_profile}" #.aws/credentials
   }

//Run number of test case based on count value: say 3 will run 3 test cases 
variable "instance_count" {
 default = 3
}

variable "hostnames" {
   default = ["wrf_testcase_1.sh", "wrf_testcase_2.sh", "wrf_testcase_3.sh","wrf_testcase_4.sh","wrf_testcase_5.sh","wrf_testcase_6.sh","wrf_testcase_7.sh", "wrf_testcase_8.sh","wrf_testcase_9.sh","wrf_testcase_10.sh"]
}

data "template_file" "user-data" {
     count = "${length(var.hostnames)}"
     template = "${file("${element(var.hostnames, count.index)}")}"
}

resource "aws_instance" "application" {
  count                         = "${var.instance_count}"
  ami                           = "${var.ami}"
  availability_zone             = "${var.availability_zone}"
  ebs_optimized                 = "${var.ebs_optimized}"
  instance_type                 = "${var.instance_type}"
  key_name                      = "${var.key_name}"
  monitoring                    = "${var.monitoring}"
  vpc_security_group_ids        = ["${var.security_group_ids}"]
  subnet_id                     = "${var.subnet_id}"
  associate_public_ip_address   = "${var.associate_public_ip_address}"
  iam_instance_profile          = "${var.iam_instance_profile}"
  user_data                     = "${element(data.template_file.user-data.*.rendered, count.index)}"
  tags                          = "${merge(var.tags, map("Name", format("%s", var.instance_name)))}"

}
