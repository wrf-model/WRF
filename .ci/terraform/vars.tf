variable "aws_profile" {
  description = "Name of the public subnet of az"
  default     = "default"
}

variable "region" {
  description = "Region for launching subnets"
  default     = "us-east-1"
}


# EC2 variables

variable "instance_name"               { default = "wrf-test" }
variable "ami"                         { default = "ami-085679d969c25b558" }
variable "availability_zone"           { default = ""   }
variable "ebs_optimized"               { default = false  }
variable "instance_type"               { default = "c5.2xlarge" }
variable "key_name"                    { default = "jenkins" }
variable "monitoring"                  { default = false  }
variable "security_group_ids"          { default = ["sg-0dfbfc9d0b4b1b519"] }
variable "subnet_id"                   { default = "subnet-010ff527a8af9ab9e" }
variable "associate_public_ip_address" { default = false  }
variable "iam_instance_profile"        { default = "" }
variable "user_data"                   { default = " "}
variable "tags"                        { default = {} }
