#pragma once

#include <fmt/format.h>

#include <array>
#include <filesystem>
#include <iostream>
#include <memory>
#include <string>
#include <string_view>
#include <vector>

#include "flexi_cfg/config/actions.h"
#include "flexi_cfg/config/classes.h"
#include "flexi_cfg/config/exceptions.h"
#include "flexi_cfg/config/helpers.h"
#include "flexi_cfg/logger.h"
#include "flexi_cfg/utils.h"

namespace flexi_cfg {

class Reader {
 public:
  explicit Reader(config::types::CfgMap cfg, const std::string& parent = "");
  ~Reader() = default;

  Reader(const Reader&) = default;
  auto operator=(const Reader&) -> Reader& = default;
  Reader(Reader&&) = default;
  auto operator=(Reader&&) -> Reader& = default;

  void dump() const;

  auto exists(const std::string& key) const -> bool;

  auto keys() const -> std::vector<std::string>;

  template <typename T>
  auto getValue(const std::string& name) const -> T;

  template <typename T>
  void getValue(const std::string& name, T& value) const;

  template <typename T>
  void getValue(const std::string& name, std::vector<T>& value) const;

  template <typename T, size_t N>
  void getValue(const std::string& name, std::array<T, N>& value) const;

 protected:
  auto getNestedConfig(const std::string& key) const
      -> std::pair<std::string, const config::types::CfgMap&>;

 private:
  Reader() = default;

  static void convert(const std::string& value_str, config::types::Type type, float& value);
  static void convert(const std::string& value_str, config::types::Type type, double& value);
  static void convert(const std::string& value_str, config::types::Type type, int& value);
  static void convert(const std::string& value_str, config::types::Type type, int64_t& value);
  static void convert(const std::string& value_str, config::types::Type type, bool& value);
  static void convert(const std::string& value_str, config::types::Type type, std::string& value);

  void getValue(const std::string& name, Reader& reader) const;

  // All of the config data!
  config::types::CfgMap cfg_data_;
  // Store the name of the parent struct for debugging/printing
  std::string parent_name_;
};

template <typename T>
auto Reader::getValue(const std::string& name) const -> T {
  T value{};
  getValue(name, value);
  return value;
}

template <typename T>
void Reader::getValue(const std::string& name, T& value) const {
  // Split the key into parts
  const auto keys = utils::split(name, '.');

  const auto cfg_val = config::helpers::getConfigValue(cfg_data_, keys);

  const auto value_ptr = dynamic_pointer_cast<config::types::ConfigValue>(cfg_val);
  const auto value_str = value_ptr->value;
  convert(value_str, value_ptr->type, value);
  logger::debug(" -- Type is {}", typeid(T).name());
}

template <typename T>
void Reader::getValue(const std::string& name, std::vector<T>& value) const {
  // Split the key into parts
  const auto keys = utils::split(name, '.');

  const auto cfg_val = config::helpers::getConfigValue(cfg_data_, keys);

  // Ensure this is a list if the user is asking for a list.
  if (cfg_val->type != config::types::Type::kList) {
    THROW_EXCEPTION(config::InvalidTypeException,
                    "Expected '{}' to contain a list, but is of type {}",
                    utils::makeName(parent_name_, name), cfg_val->type);
  }

  const auto& list = dynamic_pointer_cast<config::types::ConfigList>(cfg_val)->data;
  for (const auto& e : list) {
    const auto value_ptr = dynamic_pointer_cast<config::types::ConfigValue>(e);
    const auto value_str = value_ptr->value;
    T v{};
    convert(value_str, value_ptr->type, v);
    value.emplace_back(v);
  }
}

template <typename T, size_t N>
void Reader::getValue(const std::string& name, std::array<T, N>& value) const {
  // Split the key into parts
  const auto keys = utils::split(name, '.');

  const auto cfg_val = config::helpers::getConfigValue(cfg_data_, keys);

  // Ensure this is a list if the user is asking for a list.
  if (cfg_val->type != config::types::Type::kList) {
    THROW_EXCEPTION(config::InvalidTypeException,
                    "Expected '{}' to contain a list, but is of type {}",
                    utils::makeName(parent_name_, name), cfg_val->type);
  }

  const auto& list = dynamic_pointer_cast<config::types::ConfigList>(cfg_val)->data;
  if (list.size() != N) {
    THROW_EXCEPTION(std::runtime_error,
                    "While reading '{}': Expected {} entries in '{}', but found {}!",
                    utils::makeName(parent_name_, name), N, cfg_val, list.size());
  }
  for (size_t i = 0; i < N; ++i) {
    const auto value_ptr = dynamic_pointer_cast<config::types::ConfigValue>(list[i]);
    const auto value_str = value_ptr->value;
    convert(value_str, value_ptr->type, value[i]);
  }
}

}  // namespace flexi_cfg
