-- MySQL Script generated by MySQL Workbench
-- Wed Apr  4 16:41:48 2018
-- Model: New Model    Version: 1.0
-- MySQL Workbench Forward Engineering

SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0;
SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0;
SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='TRADITIONAL,ALLOW_INVALID_DATES';

-- -----------------------------------------------------
-- Schema Monarch
-- -----------------------------------------------------
-- Table storing multiple disease ontology ids for Monarch Initiative

-- -----------------------------------------------------
-- Schema Monarch
--
-- Table storing multiple disease ontology ids for Monarch Initiative
-- -----------------------------------------------------
CREATE SCHEMA IF NOT EXISTS `Monarch` DEFAULT CHARACTER SET utf8 ;
USE `Monarch` ;

-- -----------------------------------------------------
-- Table `Monarch`.`EFO_entryId`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `Monarch`.`EFO_entryId` (
  `DB` VARCHAR(45) NOT NULL COMMENT 'Name original database/ontology',
  `id` VARCHAR(45) NOT NULL COMMENT 'Disease ontology identifier from EFO',
  PRIMARY KEY (`DB`, `id`))
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Monarch`.`EFO_crossId`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `Monarch`.`EFO_crossId` (
  `DB1` VARCHAR(45) NOT NULL COMMENT 'Name database for id1',
  `id1` VARCHAR(45) NOT NULL COMMENT 'disease ontology identifier',
  `DB2` VARCHAR(45) NULL COMMENT 'Name database id2',
  `id2` VARCHAR(45) NULL COMMENT 'Crossreference disease ontology id to id1',
  PRIMARY KEY (`DB1`, `id1`),
  INDEX `fk_crossId_entryId_idx` (`DB1` ASC, `id1` ASC),
  CONSTRAINT `fk_crossId_entryId`
    FOREIGN KEY (`DB1` , `id1`)
    REFERENCES `Monarch`.`EFO_entryId` (`DB` , `id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Monarch`.`EFO_parentId`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `Monarch`.`EFO_parentId` (
  `DB` VARCHAR(45) NOT NULL COMMENT 'Database for id',
  `id` VARCHAR(45) NOT NULL COMMENT 'Disease ontology identifier from EFO',
  `pDB` VARCHAR(45) NULL COMMENT 'Name database for parent id',
  `parent` VARCHAR(45) NULL COMMENT 'Parent ontology for id in EFO',
  PRIMARY KEY (`DB`, `id`),
  CONSTRAINT `fk_table1_entryId1`
    FOREIGN KEY (`DB` , `id`)
    REFERENCES `Monarch`.`EFO_entryId` (`DB` , `id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Monarch`.`EFO_idNames`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `Monarch`.`EFO_idNames` (
  `DB` VARCHAR(45) NOT NULL COMMENT 'Name original database',
  `id` VARCHAR(45) NOT NULL COMMENT 'Disease ontology identifier from EFO',
  `name` VARCHAR(45) NULL COMMENT 'Term (synonym or label) to describe the disease',
  `canonical` TINYINT NULL COMMENT 'Current label for the entry',
  PRIMARY KEY (`DB`, `id`),
  CONSTRAINT `fk_table1_entryId2`
    FOREIGN KEY (`DB` , `id`)
    REFERENCES `Monarch`.`EFO_entryId` (`DB` , `id`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION)
ENGINE = InnoDB;


-- -----------------------------------------------------
-- Table `Monarch`.`EFO_sourceFiles`
-- -----------------------------------------------------
CREATE TABLE IF NOT EXISTS `Monarch`.`EFO_sourceFiles` (
  `url` VARCHAR(45) NOT NULL,
  `current` VARCHAR(45) NULL,
  PRIMARY KEY (`url`))
ENGINE = InnoDB;


SET SQL_MODE=@OLD_SQL_MODE;
SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS;
SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS;